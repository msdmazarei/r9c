defmodule GatewayCore.Outputs.Red9CobraSimpleOutGW do
  @moduledoc false
  require Logger
  require Utilities.Logging
  require DatabaseEngine.DurableQueue
  require DatabaseEngine.Models.SMS.Helper

  alias Utilities.Logging
  alias DatabaseEngine.DurableQueue
  alias KafkaEx.Protocol.Fetch.Message

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
                  consumer_sup_pid: nil
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

        Logging.debug("Returns: ~p", [rtn])
        rtn
      end

      def handle_message_set(
            message_set,
            state = %State{internal_state: internal_state, success_Q: success_Q, fail_Q: fail_Q}
          ) do
        received_items =
          message_set
          |> Enum.map(fn %Message{value: message} ->
            deserialized_message =
              message
              |> DurableQueue.deserialize()

            Logging.debug("Message:~p Deserialized: ~p ", [message, deserialized_message])
            deserialized_message
          end)

        sms_list =
          received_items
          |> Enum.filter(fn x -> is_map(x) and x.__struct__ == DatabaseEngine.Models.SMS end)

        otp_list =
          received_items
          |> Enum.filter(fn x -> is_map(x) and x.__struct__ == DatabaseEngine.Models.OTP.VAS end)

        charge_list =
          received_items
          |> Enum.filter(fn x ->
            is_map(x) and x.__struct__ == DatabaseEngine.Models.Charge.VAS
          end)

        {new_internal_state, sending_result} = send_sms_list(sms_list, internal_state)
        {new_internal_state, otp_send_result} = send_otp_list(otp_list, internal_state)
        {new_internal_state, charge_send_result} = send_charge_list(charge_list, internal_state)

        Logging.debug("internal_state:~p sms_result:~p", [internal_state, sending_result])
        enqueue_items(sms_list, sending_result, state)
        Logging.debug("internal_state:~p otp_result:~p", [internal_state, otp_send_result])
        enqueue_items(otp_list, otp_send_result, state)
        Logging.debug("internal_state:~p  charge_result:~p", [internal_state, charge_send_result])
        enqueue_items(charge_list, charge_send_result, state)

        {
          :async_commit,
          %State{
            state
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
        case nodes_to_run() |> Enum.member?(node()) do
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
        sms = case sms do
          %DatabaseEngine.Models.SMS{} ->
          DatabaseEngine.Models.SMS.Helper.describe_stage(sms,__MODULE__,"queue_to_send")
          _ -> sms
        end
        DatabaseEngine.DurableQueue.enqueue(in_Q, sms)
      end

      defp enqueue_items([], _, _) do
      end

      defp enqueue_items(
             items,
             send_results,
             state = %State{internal_state: internal_state, success_Q: success_Q, fail_Q: fail_Q}
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
