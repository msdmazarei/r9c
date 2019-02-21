defmodule ProcessManager.UnitProcess.GeneralUnitProcess do
  defmacro __using__(_ \\ []) do
    quote do
      require Logger

      require Utilities.Logging
      alias Utilities.Logging

      require Utilities
      require DatabaseEngine.Models.Utils

      require DatabaseEngine.Utils.EventLogger
      alias DatabaseEngine.Utils.EventLogger

      alias DatabaseEngine.Models.Utils, as: ModelUtils

      use GenServer

      @module_config Application.get_env(:process_manager, __MODULE__) || Application.get_env(:process_manager, ProcessManager.UnitProcess)
      @wait_to_new_message_timeout_to_hibernate @module_config[
                                                  :wait_to_new_message_timeout_to_hibernate
                                                ]
      @wait_to_new_message_timeout_to_terminate @module_config[
                                                  :wait_to_new_message_timeout_to_terminate
                                                ]

      @check_last_arrived_message_time :check_last_arrived_message_time

      defmodule State do
        @module_config Application.get_env(:process_manager, __MODULE__) || Application.get_env(:process_manager, ProcessManager.UnitProcess)

        @derive Jason.Encoder
        defstruct(
          last_arrived_message_time: nil,
          last_10_processed_messages: [],
          queues: %{
            success_Q: @module_config[:success_Q],
            fail_Q: @module_config[:fail_Q],
            cel_logging_Q: @module_config[:cel_logging_Q]
          },
          cel_script_limits: %{
            run_timeout: @module_config[:cel_script_limitation][:run_timeout],
            http_call_limit: @module_config[:cel_script_limitation][:http_call_limit]
          }
        )
      end

      def start_link(state, opts) do
        GenServer.start_link(__MODULE__, state, opts)
      end

      # def init(args,_) do
      #   Logging.debug("cqlleed.",[])
      #   init(args)
      # end
      def init(args) do
        Logging.debug("called.",[])

        watchdog()

        init_state = %State{
          last_arrived_message_time: Utilities.now()}

        state =
          Enum.reduce(Map.keys(init_state), init_state, fn item, s ->
            arg_item = Map.get(args, item)

            if arg_item == nil do
              s
            else
              Map.put(s, item, arg_item)
            end
          end)

        Logging.debug("initialize state: ~p", [state])
        EventLogger.log_event(nil, nil, "init", state)
        {:ok, state}
      end

      def send_to_queue(result, msg, queue_name)
      when is_binary(queue_name) or is_bitstring(queue_name)
      do
        case DatabaseEngine.DurableQueue.enqueue(queue_name, %{
               "type" => "script_result",
               "module" => __MODULE__,
               "msg" => msg,
               "script_result" => result,
               "time" => Utilities.now()
             }) do
          :nok ->
            Logging.warn("problem to enqueue to Q: ~p", [queue_name])

          _ ->
            :ok
        end
      end

      def send_to_queue(
             {:return, result},
             msg,
             %State{queues: %{success_Q: queue_name}}
           ) do
        Logging.debug("Called - :success , result is: ~p", [result])
        send_to_queue(result, msg, queue_name)
      end

      def send_to_queue(
             {:error, result},
             msg,
             %State{queues: %{fail_Q: queue_name}}
           ) do
        Logging.debug("Called - :error, result is:~p", [result])

        result =
          if is_map(result) do
            if Map.get(result, :__exception__) != nil do
              :io_lib.format("~p", [result])
            else
              result
            end
          else
            result
          end

        send_to_queue(result, msg, queue_name)
      end

      def update_last_10_processed_messages(state, msg) do
        %State{
          state
          | last_10_processed_messages:
              if state.last_10_processed_messages |> length < 10 do
                state.last_10_processed_messages ++ [msg.id]
              else
                (state.last_10_processed_messages
                 |> Enum.slice(1, length(state.last_10_processed_messages) - 1)) ++ [msg.id]
              end
        }
      end

      def handle_call(:alive, _, state) do
        {:reply, true, state}
      end

      def handle_call(
            {:echo, msg},
            _,
            state
          ) do
        Logging.debug("~p Called, Echo Message:~p", [state, msg])
        state = %State{state | last_arrived_message_time: Utilities.now()}
        {:reply, msg, state}
      end

      def handle_call(
            {:ingress, msg},
            _from,
            state = %State{}
          ) do
        identifier = ProcessManager.UnitProcess.Identifier.get_identifier(msg)
        Logging.debug("ingress msg :~p arrived", [msg])
        Logging.debug("id:~p ", [identifier])

        rtn =
          if state.last_10_processed_messages |> Enum.member?(msg.id) do
            Logging.debug("message: ~p already processed.", [msg.id])
            {:reply, :true, state}
          else
            script = ProcessManager.UnitProcess.Identifier.get_script(msg, state)
            script_result = ProcessManager.Script.run_script(
              script,
              msg,
              state,
              %{},
              state.cel_script_limits.run_timeout || 5000
            )
            Logging.debug("script result: ~p",[script_result])

            send_to_queue(
              script_result,
              msg,
              state
            )

            state = %State{
              state
              | last_arrived_message_time: Utilities.now()
            }

            state = update_last_10_processed_messages(state, msg)
            {:reply, true, state}
          end

        Logging.debug("log_event called")

        EventLogger.log_event(
          ModelUtils.get_entity_type(msg),
          ModelUtils.get_entity_id(msg),
          "process",
          %{}
        )

        rtn
      end


      def handle_cast(_msg, state) do
        {:noreply, state}
      end

      def handle_info(
            :timeout,
            state = %State{
              last_arrived_message_time: last_arrived_message_time}
          ) do
        Logging.debug("timeout called. last_arrived_message_time:~p", [
          last_arrived_message_time
        ])

        if Utilities.now() - last_arrived_message_time > @wait_to_new_message_timeout_to_hibernate do
          Logging.debug("Enter in Hibernate Mode. Current State:~p", [ state])
          :proc_lib.hibernate(:gen_server, :enter_loop, [__MODULE__, [], state])
        end

        Logging.debug(" All thing is right, let try hibernating later.", [])
        Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)
        {:noreply, state}
      end

      def handle_info(
            @check_last_arrived_message_time,
            state = %State{last_arrived_message_time: last_arrived_message_time}
          ) do
        Logging.debug(
          " Called. msg:~p With Params Last Arrived Message:~p",
          [ @check_last_arrived_message_time, last_arrived_message_time]
        )

        if Utilities.now() - last_arrived_message_time >= @wait_to_new_message_timeout_to_terminate do
          Logging.debug(" Stop process:~p cause of no message after long time, So let stop it", [
            self()
          ])

          Process.exit(self(), :normal)
        end

        Process.send_after(
          self(),
          @check_last_arrived_message_time,
          @wait_to_new_message_timeout_to_terminate + 100
        )

        {:noreply, state}
      end

      @compile {:inline, watchdog: 0}

      def watchdog() do
        Logging.debug("called. args:~p ~p ~p",[self(),:timeout,@wait_to_new_message_timeout_to_hibernate])
        Process.send_after(
          self(),
          :timeout,
          @wait_to_new_message_timeout_to_hibernate + 100
          )

        Process.send_after(
          self(),
          @check_last_arrived_message_time,
          @wait_to_new_message_timeout_to_terminate + 100
        )
      end

      # end qoute do
    end

    # end defmacro using
  end

  # end defmodule
end
