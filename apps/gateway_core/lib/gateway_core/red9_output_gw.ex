defmodule GatewayCore.Outputs.Red9CobraSimpleOutGW do
  @moduledoc false
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  alias DatabaseEngine.DurableQueue
  alias KafkaEx.Protocol.Fetch.Message

  @callback send_sms_list([DatabaseEngine.Models.SMS], any()) :: {any(), list(boolean())}

  @callback gw_queue_list() :: [in_Q: String.t(), success_Q: String.t(), fail_Q: String.t()]
  @callback gw_init() :: any


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
                  consumer_sup_pid: :nil
      end

      def init(topic, partition) do
        Logging.debug("Called", [])
        Logging.debug("Calling [gw_queue_list]", [])
        queues = gw_queue_list()


        rtn = {
          :ok,
          %State {
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
        sms_list = message_set
                   |> Enum.map(
                        fn %Message{value: message} ->
                          deserialized_message = message
                                                 |> DurableQueue.deserialize()
                          Logging.debug("Message:~p Deserialized: ~p ", [message, deserialized_message])
                          deserialized_message = for {key, val} <- deserialized_message,
                                                     into: %{}, do: {String.to_atom(key), val}
                          msg = struct(%DatabaseEngine.Models.SMS{}, deserialized_message)
                        end
                      )
        {new_internal_state, sending_result} =
        send_sms_list(sms_list, internal_state)
        Logging.debug("internal_state:~p sms_result:~p", [internal_state, sending_result])

        for i <- 1..length(sending_result) do
          sms_ = :lists.nth(i, sms_list)

          if :lists.nth(i, sending_result) == true do
            if success_Q != :nil do
              DatabaseEngine.DurableQueue.enqueue(success_Q, sms_)
            else
              Logging.debug("No Success Q Defined, to push successful sms", [])
            end
          else
            if fail_Q != :nil do
              DataBaseEngine.DurableQueue.enqueue(fail_Q, sms_)
            else
              Logging.debug("No Fail Q Defined. to push failuar sms", [])
            end
          end
        end



        {
          :async_commit,
          %State{
            state |
            internal_state: internal_state
          }
        }
      end

      @spec start(String.t(), String.t()) :: {:ok, pid()} | {:nok, :no_in_Q_defined}
      def start(:nil, _any) do
        {:nok, :no_in_Q_defined}
      end

      def start(q_name, :nil) do
        start(q_name, q_name <> "_consumer")
      end

      def start(q_name, consumer_name) do
        r = DatabaseEngine
        .DurableQueue
        .start_consumer_group(
          q_name,
          consumer_name,
          __MODULE__
        )
        case r do
          {:ok, pid} when is_pid(pid) ->
            Process.register(pid, __MODULE__)
            r
          _ -> r
        end


      end

      @spec start() :: {:ok, pid()} | {:nok, :no_in_Q_defined}
      def start() do
        queues = gw_queue_list()
        start(queues[:in_Q], :nil)
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
        if (pid != :nil) do
          stop(pid) end
      end

      def send_sms(sms) do
        queues = gw_queue_list()
        in_Q = queues[:in_Q]
        DatabaseEngine.DurableQueue.enqueue(in_Q, sms)
      end


    end


  end
end
