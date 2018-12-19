defmodule GatewayCore.Outputs.Red9CobraSimpleOutGW do
  @moduledoc false
  require Logger
  require Utilities.Logging
  require DatabaseEngine.DurableQueue
  require DatabaseEngine.Models.SMS.Helper
  require Utilities.Circular

  alias Utilities.Logging
  alias DatabaseEngine.DurableQueue
  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.Models.SMS

  @callback send_charge_list([DatabaseEnginge.Models.Charge.VAS], any()) ::
              {any(), list(boolean())}
  @callback send_sms_list([DatabaseEngine.Models.SMS], any()) :: {any(), list(boolean())}
  @callback send_otp_list([DatabaseEngine.Models.OTP.VAS], any()) :: {any(), list(boolean())}
  @callback gw_queue_list() :: [in_Q: String.t(), success_Q: String.t(), fail_Q: String.t()]
  @callback gw_init() :: any
  @callback nodes_to_run() :: list(String.t())

  defmacro __using__(_ \\ []) do
    #    use KafkaEx.GenConsumer
    quote do
      use KafkaEx.GenConsumer
      require Logger
      require Utilities.Logging

      defmodule State do
        defstruct partition: 0,
                  topic: "",
                  in_Q: "",
                  success_Q: "",
                  fail_Q: "",
                  internal_state: %{},
                  consumer_sup_pid: nil,
                  circular_buffer: Utilities.Circular.new_circular_buffer(1000),
                  failed_cb: Utilities.Circular.new_circular_buffer(1000)
      end

      def init(topic, partition) do
        Logging.debug("Called", [])
        Logging.debug("Calling [gw_queue_list]", [])
        queues = gw_queue_list()

        rtn = {
          :ok,
          %State{
            partition: partition,
            topic: topic,
            in_Q: queues[:in_Q],
            success_Q: queues[:success_Q],
            fail_Q: queues[:fail_Q],
            internal_state: gw_init()
          }
        }

        #        Logging.debug("Returns: ~p", [rtn])
        rtn
      end

      @time_to_do_key "__time_to_do"
      @retry_count "__retry_count"
      @max_retry_count 3
      @retry_delay 1000
      @a_second_to_sleep 1000


      def message_time_is_ok_to_send_right_now(
            message = %{options: %{@time_to_do_key => time_to_do}},
            state
          ) do
#        Logging.debug("Called. message:~p", [message])

        rtn =
          if Utilities.now() > time_to_do do
            true
          else
            false
          end

#        Logging.debug("returns:~p", [rtn])
        rtn
      end

      def message_time_is_ok_to_send_right_now(_, _) do
#        Logging.debug("Not Matched and returns always true, args:~p",[m])
        true
      end

      def get_true_message_list_to_send(
            message_list,
            state = %State{topic: topic, partition: partition, failed_cb: failed_cb}
          ) do
        failed_cb_array = Utilities.Circular.get_array(failed_cb)

        to_enqueue =
          message_list |> Enum.filter(fn x -> !message_time_is_ok_to_send_right_now(x, state) end)

        true_messages =
          message_list |> Enum.filter(fn x -> message_time_is_ok_to_send_right_now(x, state) end)

        to_enqueue |> Enum.map(fn x -> :ok = DurableQueue.enqueue(topic, partition, x) end)

        drop_message_func = fn x ->
          options = x.options
          retry_count = options[@retry_count] || 0

          if retry_count > @max_retry_count do
            true
          else
            false
          end
        end

        is_failed_recently = fn x ->
          msg_id = get_message_id(x)
          {msg_try_count, msg_try_time} = get_try_count_time(x)

          {old_try_count, old_sent_time} =
            Utilities.Circular.get_value(failed_cb, msg_id) || {-2, -2}

          rtn_ =
            if msg_try_time > old_sent_time do
              false
            else
              old_try_count >= msg_try_count
            end

          Logging.debug(
            "is_failed_recently returns:~p old_time:~p, new_time:~p old_try:~p msg_try_count:~p",
            [rtn_, old_sent_time, msg_try_time, old_try_count, msg_try_count]
          )

          rtn_
        end

        drop_messages = true_messages |> Enum.filter(fn x -> drop_message_func.(x) end)

        true_messages =
          true_messages
          |> Enum.filter(fn x -> !drop_message_func.(x) end)
          |> Enum.filter(fn x -> !is_failed_recently.(x) end)

          Logging.debug("true messages count:~p",[true_messages|>length()])

        drop_messages
        |> Enum.map(fn x ->
          Logging.warn("MSG ID DROPPED CAUSE OF RETRY_COUNT:~p", [get_message_id(x)])
        end)

        if length(to_enqueue) > 0 do
          Logging.debug("let sleep for a second")
          Process.sleep(@a_second_to_sleep)
        end

        true_messages
      end

      def try_fails([],[],_) do

      end
      def try_fails(messages, statuses, state = %State{topic: topic, partition: partition}) do
        Logging.debug("called message_status:~p", [statuses])

        failed_messages =
          messages
          |> Enum.zip(statuses)
          |> Enum.filter(fn {m, s} -> !s end)
          |> Enum.map(fn {m, s} -> m end)

        Logging.debug("calculate requeuable messages")

        requeuable =
          failed_messages
          |> Enum.filter(fn x ->
            x = Map.from_struct(x)

            case x do
              %{options: options} ->
                case options do
                  %{@retry_count => retry_count} when retry_count < @max_retry_count ->
                    true

                  %{} ->
                    true

                  _ ->
                    false
                end

              _ ->
                false
            end
          end)

        Logging.debug("calculating final messages")

        final_messages =
          requeuable
          |> Enum.map(fn x ->
            options = x.options
            old_retry_count = options[@retry_count] || 0

            new_options =
              options
              |> Map.put(@retry_count, old_retry_count + 1)
              |> Map.put(@time_to_do_key, Utilities.now() + @retry_delay)

            #            x |> Map.put(:options, new_options)
            %{x | options: new_options}
          end)

        final_messages
        |> Enum.map(fn x ->
          Logging.debug("enqueue message:~p for later try", [x])
          DurableQueue.enqueue(topic, partition, x)
        end)
      end

      @compile {:inline, get_try_count_time: 1}
      def get_try_count_time(message = %SMS{}) do
        options = message.options || %{}
        {options[@retry_count] || -1, options[@time_to_do_key] || -1 }
      end

      def get_try_count_time(_) do
        {-2, -2}
      end

      @compile {:inline, get_message_id: 1}

      def get_message_id(message = %SMS{}) do
        #        Logging.debug("Called message - sms : ~p", [message])
        message.id
      end

      def get_message_id(message = %{}) do
        #        Logging.debug("Called message - map: ~p", [message])

        message[:id] || message["id"] || nil
      end

      def get_message_id(message) do
        #        Logging.debug("Called unknown message type:~p", [message])
        nil
      end

      def update_state_by_success_messages_id(
            messages,
            statuses,
            state = %State{circular_buffer: cb, failed_cb: failed_cb}
          ) do
        success_messages =
          messages
          |> Enum.zip(statuses)
          |> Enum.filter(fn {m, s} -> s end)
          |> Enum.map(fn {m, _} -> m end)

        failed_messages =
          messages
          |> Enum.zip(statuses)
          |> Enum.filter(fn {m, s} -> !s end)
          |> Enum.map(fn {m, _} -> m end)

        new_cb =
          success_messages
          |> Enum.reduce(cb, fn item, acc ->
            item_id = get_message_id(item)
            Utilities.Circular.append_to_circular_buffer(acc, item_id)
          end)

        new_failed_cb =
          failed_messages
          |> Enum.reduce(failed_cb, fn item, acc ->
            item_id = get_message_id(item)
            {try_count,try_time} = get_try_count_time(item)
            {old_count,old_time} = case  Utilities.Circular.get_value(acc,item_id) do
              nil -> {-1,-2}
                     {old_count,old_time} -> {old_count,old_time}
            end
            try_count = Enum.max([try_count,old_count])
            try_time = Enum.max([old_time,try_time])

            b=Utilities.Circular.append_to_circular_buffer(acc, item_id, {try_count,try_time})
#            Logging.debug("********** AFTR APPENDING:~p",[ b |> Utilities.Circular.get_dict()])
            b
          end)
#          Logging.debug("old_faild_dict:~p failed_dict:~p",[failed_cb|> Utilities.Circular.get_dict(), new_failed_cb |> Utilities.Circular.get_dict()])

        %State{
          state
          | circular_buffer: new_cb,
            failed_cb: new_failed_cb
        }
      end

      def handle_message_set(
            message_set,
            state = %State{
              internal_state: internal_state,
              success_Q: success_Q,
              fail_Q: fail_Q,
              partition: partition,
              topic: topic,
              circular_buffer: cb
            }
          ) do
        Logging.debug("Called with message_set count:~p", [message_set |> length])

        received_items =
          message_set
          |> Enum.map(fn %Message{value: message} ->
            deserialized_message =
              message
              |> DurableQueue.deserialize()

            #            Logging.debug("Message:~p Deserialized: ~p ", [message, deserialized_message])
            Logging.debug("Message:~p", [message])
            deserialized_message
          end)

        Logging.debug("unique received_items")

        received_items =
          get_true_message_list_to_send(received_items, state)
          Logging.debug("after check message true message_count:~p",[received_items|> length])

          received_items = received_items           |> Enum.uniq_by(fn x -> get_message_id(x) end)



        Logging.debug("after uniqurness, message set count:~p", [
          received_items |> length
        ])

        #          received_items
        #          |> Enum.filter(fn x -> message_time_is_ok_to_send_right_now(x, state) end)

        sent_items = Utilities.Circular.get_array(cb)
        Logging.debug("filter received_items to not sent items.")

        received_items =
          received_items
          |> Enum.filter(fn item ->
            item_id = get_message_id(item)
            Logging.debug("check did we send it before :~p", [item_id])
            rtn = !Enum.member?(sent_items, item_id)
            Logging.debug("is it dupplication check result :~p", [rtn])
            rtn
          end)

        Logging.debug("message count after  removing sent items:~p", [received_items |> length])

        sms_list =
          received_items
          |> Enum.filter(fn x ->
            is_map(x) and Map.has_key?(x, :__struct__) and
              x.__struct__ == DatabaseEngine.Models.SMS
          end)

        Logging.debug("sms_list count : ~p", [sms_list |> length])

        otp_list =
          received_items
          |> Enum.filter(fn x ->
            is_map(x) and Map.has_key?(x, :__struct__) and
              x.__struct__ == DatabaseEngine.Models.OTP.VAS
          end)

        Logging.debug("otp_list count:~p", [otp_list |> length])

        charge_list =
          received_items
          |> Enum.filter(fn x ->
            is_map(x) and Map.has_key?(x, :__struct__) and
              x.__struct__ == DatabaseEngine.Models.Charge.VAS
          end)

        Logging.debug("charge_list count:~p", [charge_list |> length])

        {new_internal_state, sending_result} = send_sms_list(sms_list, internal_state)
        try_fails(sms_list, sending_result, state)
        new_state = update_state_by_success_messages_id(sms_list, sending_result, state)

        {new_internal_state, otp_send_result} = send_otp_list(otp_list, internal_state)
        try_fails(otp_list, otp_send_result, state)
        new_state = update_state_by_success_messages_id(otp_list, otp_send_result, new_state)


        {new_internal_state, charge_send_result} = send_charge_list(charge_list, internal_state)
        try_fails(charge_list, charge_send_result, state)

        new_state =
          update_state_by_success_messages_id(charge_list, charge_send_result, new_state)

        Logging.debug("internal_state:~p sms_result:~p", [internal_state, sending_result])
        enqueue_items(sms_list, sending_result, state)
        Logging.debug("internal_state:~p otp_result:~p", [internal_state, otp_send_result])
        enqueue_items(otp_list, otp_send_result, state)
        Logging.debug("internal_state:~p  charge_result:~p", [internal_state, charge_send_result])
        enqueue_items(charge_list, charge_send_result, state)

        {
          :async_commit,
          %State{
            new_state
            | internal_state: internal_state
          }
        }
      end

      @spec start(String.t(), String.t()) :: {:ok, pid()} | {:nok, :no_in_Q_defined}
      def start(nil, _any) do
        {:nok, :no_in_Q_defined}
      end

      def start(q_name, nil) do
        start(q_name, q_name <> "_consumer")
      end

      def start(q_name, consumer_name) do
        r =
          DatabaseEngine.DurableQueue.start_consumer_group(
            q_name,
            consumer_name,
            __MODULE__
          )

        case r do
          {:ok, pid} when is_pid(pid) ->
            Process.register(pid, __MODULE__)
            r

          _ ->
            r
        end
      end

      def start_link() do
      end

      @spec start() :: {:ok, pid()} | {:nok, :no_in_Q_defined} | {:nok, :this_is_not_target_node}

      def start() do
        case (nodes_to_run() || []) |> Enum.member?(node()) do
          true ->
            queues = gw_queue_list()
            start(queues[:in_Q], nil)

          false ->
            Logging.warn("Dont Start ~p On ~p, see configuration", [__MODULE__, node()])
            {:nok, :this_is_not_target_node}
        end
      end

      @spec start(String.t()) :: {:ok, pid()} | {:nok, :no_in_Q_defined}
      def start(consumer_name) do
        queues = gw_queue_list()
        start(queues[:in_Q], consumer_name)
      end

      def stop(consumer_sup_pid) do
        DatabaseEngine.DurableQueue.stop_consumer_group(consumer_sup_pid)
      end

      def stop() do
        pid = Process.whereis(__MODULE__)
        Logging.debug("PID:~p~n", [pid])

        if pid != nil do
          stop(pid)
        end
      end

      def send(sms) do
        queues = gw_queue_list()
        in_Q = queues[:in_Q]

        sms =
          case sms do
            %DatabaseEngine.Models.SMS{} ->
              DatabaseEngine.Models.SMS.Helper.describe_stage(sms, __MODULE__, "queue_to_send")

            _ ->
              sms
          end

        DatabaseEngine.DurableQueue.enqueue(in_Q, sms)
      end

      defp enqueue_items([], _, _) do
      end

      defp enqueue_items(
             items,
             send_results,
             state = %State{
               internal_state: internal_state,
               success_Q: success_Q,
               fail_Q: fail_Q,
               circular_buffer: cb
             }
           ) do
        for i <- 1..length(items) do
          item = :lists.nth(i, items)

          if :lists.nth(i, send_results) == true do
            if success_Q != nil do
              item =
                case item do
                  %DatabaseEngine.Models.SMS{} ->
                    DatabaseEngine.Models.SMS.Helper.describe_stage(
                      item,
                      __MODULE__,
                      "successfully_sent"
                    )

                  _ ->
                    item
                end

              case DatabaseEngine.DurableQueue.enqueue(success_Q, item) do
                :nok ->
                  Logging.warn("Could not enqueue id:~p into success_Q:~p", [
                    item.id,
                    success_Q
                  ])

                _ ->
                  :ok
              end
            else
              Logging.debug("No Success Q Defined, to push successful sms", [])
            end
          else
            if fail_Q != nil do
              item =
                case item do
                  %DatabaseEngine.Models.SMS{} ->
                    DatabaseEngine.Models.SMS.Helper.describe_stage(
                      item,
                      __MODULE__,
                      "failed_to_send"
                    )

                  _ ->
                    item
                end

              case DatabaseEngine.DurableQueue.enqueue(fail_Q, item) do
                :nok ->
                  Logging.warn("Could not enqueue sms.id;~p into fail_Q:~p", [item.id, fail_Q])

                _ ->
                  :ok
              end
            else
              Logging.debug("No Fail Q Defined. to push failuar sms", [])
            end
          end
        end
      end
    end
  end
end
