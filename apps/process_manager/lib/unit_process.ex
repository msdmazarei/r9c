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

      @module_config Application.get_env(:process_manager, __MODULE__) ||
                       Application.get_env(:process_manager, ProcessManager.UnitProcess)
      @wait_to_new_message_timeout_to_hibernate @module_config[
                                                  :wait_to_new_message_timeout_to_hibernate
                                                ]
      @wait_to_new_message_timeout_to_terminate @module_config[
                                                  :wait_to_new_message_timeout_to_terminate
                                                ]

      @check_last_arrived_message_time :check_last_arrived_message_time

      defmodule State do
        @module_config Application.get_env(:process_manager, __MODULE__) ||
                         Application.get_env(:process_manager, ProcessManager.UnitProcess)

        @derive Jason.Encoder
        defstruct(
          process_name: nil,
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
          },
          last_cel_result: nil,
          script_imutable_vars: nil,
          processed_messages: 0,
          arrived_messages: 0,
          repeated_messages: 0
        )
      end

      def delete_process_stat_data(state) do
        process_key = "#{state.process_name}_unit_process_stat"
        DatabaseEngine.Interface.LProcessData.del(process_key)
      end

      def update_process_stat_data(state) do
        process_key = "#{state.process_name}_unit_process_stat"

        data = %{
          "processed_messages" => state.processed_messages,
          "arrived_messages" => state.arrived_messages,
          "last_arrived_message_time" => state.last_arrived_message_time,
          "repeated_messages" => state.repeated_messages
        }

        DatabaseEngine.Interface.LProcessData.set(process_key, data)
      end

      def start_link(state, opts) do
        GenServer.start_link(__MODULE__, state, opts)
      end

      # def init(args,_) do
      #   Logging.debug("cqlleed.",[])
      #   init(args)
      # end
      def init(args) do
        Logging.debug("called.", [])

        watchdog()

        process_name =
          case args do
            %{"process_name" => pn} -> pn
            _ -> "unknown"
          end

        Process.put(:process_name, process_name)

        init_state = %State{
          last_arrived_message_time: Utilities.now(),
          process_name: process_name
        }

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
          when is_binary(queue_name) or is_bitstring(queue_name) do
        case DatabaseEngine.DurableQueue.enqueue(queue_name, %{
               "type" => "script_result",
               "module" => __MODULE__,
               "msg" => msg,
               "script_result" => Utilities.nested_tuple_to_list(result),
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
        msg_id = ProcessManager.UnitProcess.Identifier.get_identifier(msg)

        s = %State{
          state
          | last_10_processed_messages:
              if state.last_10_processed_messages |> length < 10 do
                state.last_10_processed_messages ++
                  [
                    msg_id
                  ]
              else
                (state.last_10_processed_messages
                 |> Enum.slice(1, length(state.last_10_processed_messages) - 1)) ++ [msg_id]
              end
        }

        Logging.debug("pname:~p -> processed items:~p", [
          s.process_name,
          s.last_10_processed_messages
        ])

        s
      end

      def callback(
            data,
            %DatabaseEngine.Models.InternalCallback{
              :module_name => module,
              :function_name => function,
              :arguments => arguments
            }
          ) do
        Utilities.callback(data, module, function, arguments)
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
            state = %State{
              process_name: process_name,
              processed_messages: processed_messages
            }
          ) do
        identifier = ProcessManager.UnitProcess.Identifier.get_identifier(msg)
        # Logging.debug("pname: ~p -> ingress msg :~p arrived", [process_name, msg])
        Logging.debug("pname: ~p -> id:~p ", [process_name, identifier])

        rtn =
          if state.last_10_processed_messages |> Enum.member?(msg.id) do
            Logging.debug("pname: ~p -> message: ~p already processed.", [process_name, msg.id])
            {:reply, true, state}
          else
            script = ProcessManager.UnitProcess.Identifier.get_script(msg, state)

            new_state =
              if __MODULE__.__info__(:functions)[:prepare_script_imutable_variables] != nil do
                script_im_vars =
                  Kernel.apply(__MODULE__, :prepare_script_imutable_variables, [msg, state])

                %{state | script_imutable_vars: script_im_vars}
              else
                state
              end

            additional_functions =
              if __MODULE__.__info__(:functions)[:additional_functions] != nil do
                Kernel.apply(__MODULE__, :additional_functions, [msg, state])
              else
                %{}
              end

            script_result =
              ProcessManager.Script.run_script(
                script,
                msg,
                new_state,
                additional_functions,
                state.cel_script_limits.run_timeout || 5000
              )

            Logging.debug("pname: ~p -> script result: ~p", [process_name, script_result])

            send_to_queue(
              script_result,
              msg,
              state
            )

            state = %State{
              state
              | last_arrived_message_time: Utilities.now(),
                last_cel_result: script_result,
                processed_messages: processed_messages + 1
            }

            state = update_last_10_processed_messages(state, msg)

            # Logging.debug("checking for inernal_callback ->~p", [msg.internal_callback])

            if msg.internal_callback != nil do
              callback(state, msg.internal_callback)
            end

            {:reply, true, state}
          end

        Logging.debug("pname :~p log_event called", [process_name])

        EventLogger.log_event(
          ModelUtils.get_entity_type(msg),
          ModelUtils.get_entity_id(msg),
          "process",
          %{}
        )

        rtn
      end

      def handle_cast(
            {:process_message, msg},
            state = %State{
              process_name: process_name,
              processed_messages: processed_messages
            }
          ) do
        identifier = ProcessManager.UnitProcess.Identifier.get_identifier(msg)
        # Logging.debug("pname: ~p -> ingress msg :~p arrived", [process_name, msg])
        Logging.debug("pname: ~p -> id:~p ", [process_name, identifier])

        rtn =
          if state.last_10_processed_messages |> Enum.member?(msg.id) do
            Logging.debug("pname: ~p -> message: ~p already processed.", [process_name, msg.id])

            state = %State{
              state
              | last_arrived_message_time: Utilities.now(),
                processed_messages: processed_messages + 1,
                repeated_messages: state.repeated_messages + 1
            }

            update_process_stat_data(state)
            {:noreply,  state}
          else
            script = ProcessManager.UnitProcess.Identifier.get_script(msg, state)

            new_state =
              if __MODULE__.__info__(:functions)[:prepare_script_imutable_variables] != nil do
                script_im_vars =
                  Kernel.apply(__MODULE__, :prepare_script_imutable_variables, [msg, state])

                %{state | script_imutable_vars: script_im_vars}
              else
                state
              end

            additional_functions =
              if __MODULE__.__info__(:functions)[:additional_functions] != nil do
                Kernel.apply(__MODULE__, :additional_functions, [msg, state])
              else
                %{}
              end

            script_result =
              ProcessManager.Script.run_script(
                script,
                msg,
                new_state,
                additional_functions,
                state.cel_script_limits.run_timeout || 5000
              )

            Logging.debug("pname: ~p -> script result: ~p", [process_name, script_result])

            send_to_queue(
              script_result,
              msg,
              state
            )

            state = %State{
              state
              | last_arrived_message_time: Utilities.now(),
                last_cel_result: script_result,
                processed_messages: processed_messages + 1
            }

            state = update_last_10_processed_messages(state, msg)

            # Logging.debug("checking for inernal_callback ->~p", [msg.internal_callback])

            if msg.internal_callback != nil do
              callback(state, msg.internal_callback)
            end

            update_process_stat_data(state)
            {:noreply, state}
          end

        Logging.debug("pname :~p log_event called", [process_name])

        EventLogger.log_event(
          ModelUtils.get_entity_type(msg),
          ModelUtils.get_entity_id(msg),
          "process",
          %{}
        )

        Logging.debug("returns:~p",[rtn])
        rtn
      end

      def handle_cast(_msg, state) do
        {:noreply, state}
      end

      def handle_info(
            :timeout,
            state = %State{
              process_name: process_name,
              last_arrived_message_time: last_arrived_message_time
            }
          ) do
        Logging.debug("pname:~p -> timeout called. last_arrived_message_time:~p", [
          process_name,
          last_arrived_message_time
        ])

        if Utilities.now() - last_arrived_message_time > @wait_to_new_message_timeout_to_hibernate do
          Logging.debug("pname: ~p -> Enter in Hibernate Mode. Current State:~p", [
            process_name,
            state
          ])

          :proc_lib.hibernate(:gen_server, :enter_loop, [__MODULE__, [], state])
        end

        Logging.debug("pname:~p ->  All thing is right, let try hibernating later.", [
          process_name
        ])

        Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)
        {:noreply, state}
      end

      def handle_info(
            @check_last_arrived_message_time,
            state = %State{
              process_name: process_name,
              last_arrived_message_time: last_arrived_message_time
            }
          ) do
        Logging.debug(
          " pname: ~p -> Called. msg:~p With Params Last Arrived Message:~p",
          [process_name, @check_last_arrived_message_time, last_arrived_message_time]
        )

        res =
          if Utilities.now() - last_arrived_message_time >=
               @wait_to_new_message_timeout_to_terminate do
            Logging.debug(
              "
          pname: ~p ->
          Stop process:~p cause of no message after long time, So let stop it",
              [
                process_name,
                self()
              ]
            )

            {:stop, "no message for long time", state}
          else
            {:noreply, state}

            # Process.exit(self(), :normal)
          end

        Process.send_after(
          self(),
          @check_last_arrived_message_time,
          @wait_to_new_message_timeout_to_terminate + 100
        )

        res
      end

      def handle_info({:ingress, msg, from, ref}, state) do
        Logging.debug("info ingress - msg:~p from:~p ref:~p", [
          msg,
          from,
          ref
        ])

        # send arrived message as new message to postpone processing
        GenServer.cast(self(), {:process_message, msg})

        send(from, ref)
        new_state = %State{state | arrived_messages: state.arrived_messages + 1}
        update_process_stat_data(new_state)
        {:noreply, new_state}
      end

      @compile {:inline, watchdog: 0}

      def watchdog() do
        Logging.debug("called. args:~p ~p ~p", [
          self(),
          :timeout,
          @wait_to_new_message_timeout_to_hibernate
        ])

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

      def terminate(
            reason,
            state
          ) do
        Logging.debug("Called. reason:~p, proccess name:~p", [reason, state.process_name])
        pmodel = DatabaseEngine.Interface.Process.get(state.process_name)
        Logging.debug("pmodel:~p", [pmodel])

        if pmodel != nil and pmodel.local_pid == self() do
          DatabaseEngine.Interface.Process.del(state.process_name)
          DatabaseEngine.Interface.LProcess.del(state.process_name)
          delete_process_stat_data(state)
        end

        state
      end

      # end qoute do
    end

    # end defmacro using
  end

  # end defmodule
end
