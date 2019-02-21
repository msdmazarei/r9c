# defmodule Dispatcher.Process.VAS.UserProcess do
#   @moduledoc false

#   require Logger
#   require Utilities.Logging
#   require Utilities
#   require DatabaseEngine.Models.Utils
#   require DatabaseEngine.Utils.EventLogger
#   alias DatabaseEngine.Utils.EventLogger
#   alias DatabaseEngine.Models.Utils, as: ModelUtils
#   alias Utilities.Logging
#   #  alias :luerl, as: LUA

#   use GenServer

#   @module_config Application.get_env(:dispatcher, __MODULE__)
#   @wait_to_new_message_timeout_to_hibernate @module_config[
#                                               :wait_to_new_message_timeout_to_hibernate
#                                             ]
#   @wait_to_new_message_timeout_to_terminate @module_config[
#                                               :wait_to_new_message_timeout_to_terminate
#                                             ]

#   @check_last_arrived_message_time :check_last_arrived_message_time

#   defmodule State do
#     @module_config Application.get_env(:dispatcher, Dispatcher.Process.VAS.UserProcess)

#     defstruct(
#       cell_no: nil,
#       last_arrived_message_time: nil,
#       last_10_processed_messages: [],
#       queues: %{
#         success_Q: @module_config[:success_Q],
#         fail_Q: @module_config[:fail_Q],
#         cel_logging_Q: @module_config[:cel_logging_Q]
#       },
#       cel_script_limits: %{
#         run_timeout: @module_config[:cel_script_limitation][:run_timeout],
#         http_call_limit: @module_config[:cel_script_limitation][:http_call_limit]
#       }
#     )
#   end

#   @spec start_link(any(), [
#           {:debug, [:log | :statistics | :trace | {any(), any()}]}
#           | {:hibernate_after, :infinity | non_neg_integer()}
#           | {:name, atom() | {:global, any()} | {:via, atom(), any()}}
#           | {:spawn_opt,
#              :link
#              | :monitor
#              | {:fullsweep_after, non_neg_integer()}
#              | {:min_bin_vheap_size, non_neg_integer()}
#              | {:min_heap_size, non_neg_integer()}
#              | {:priority, :high | :low | :normal}}
#           | {:timeout, :infinity | non_neg_integer()}
#         ]) :: :ignore | {:error, any()} | {:ok, pid()}
#   def start_link(state, opts) do
#     GenServer.start_link(__MODULE__, state, opts)
#   end

#   def init(args = %{"cell_no" => cell_no}) do
#     watchdog()
#     init_state = %State{cell_no: cell_no, last_arrived_message_time: Utilities.now()}

#     state =
#       Enum.reduce(Map.keys(init_state), init_state, fn item, s ->
#         arg_item = Map.get(args, item)

#         if arg_item == nil do
#           s
#         else
#           Map.put(s, item, arg_item)
#         end
#       end)

#     Logging.debug("initialize state: ~p", [state])
#     EventLogger.log_event(nil, nil, "init", %{"cell_no" => cell_no})
#     {:ok, state}
#     #    {:ok, %{"last_arrived_message_time" => Utilities.now(), "cell_no" => cell_no}}
#   end

#   defp send_to_queue(cell_no, result, sms, queue_name) do
#     case DatabaseEngine.DurableQueue.enqueue(queue_name, %{
#            "type" => "script_result",
#            "module" => __MODULE__,
#            "sms" => sms,
#            "script_result" => result,
#            "cell_no" => cell_no,
#            "time" => Utilities.now()
#          }) do
#       :nok ->
#         Logging.warn("problem to enqueue to Q: ~p", [queue_name])

#       _ ->
#         :ok
#     end
#   end

#   defp send_to_queue(
#          {:return, result},
#          sms,
#          %State{queues: %{success_Q: queue_name}, cell_no: cell_no}
#        ) do
#     Logging.debug("Called - :success , result is: ~p", [result])
#     send_to_queue(cell_no, result, sms, queue_name)
#   end

#   defp send_to_queue(
#          {:error, result},
#          sms,
#          %State{queues: %{fail_Q: queue_name}, cell_no: cell_no}
#        ) do
#     Logging.debug("Called - :error, result is:~p", [result])

#     result =
#       if is_map(result) do
#         if Map.get(result, :__exception__) != nil do
#           :io_lib.format("~p", [result])
#         else
#           result
#         end
#       else
#         result
#       end

#     send_to_queue(cell_no, result, sms, queue_name)
#   end

#   def update_last_10_processed_messages(state, sms) do
#     %State{
#       state
#       | last_10_processed_messages:
#           if state.last_10_processed_messages |> length < 10 do
#             state.last_10_processed_messages ++ [sms.id]
#           else
#             (state.last_10_processed_messages
#              |> Enum.slice(1, length(state.last_10_processed_messages) - 1)) ++ [sms.id]
#           end
#     }
#   end

#   def handle_call(:alive, _, state) do
#     {:reply, true, state}
#   end

#   def handle_call(
#         {:echo, msg},
#         _,
#         state = %State{cell_no: cell_no}
#       ) do
#     Logging.debug("~p Called, Echo Message:~p", [cell_no, msg])
#     state = %State{state | last_arrived_message_time: Utilities.now()}
#     {:reply, msg, state}
#   end

#   def handle_call(
#         {:ingress, sms = %DatabaseEngine.Models.SMS{sender: cell_no, receiver: service_no}},
#         _from,
#         state = %State{cell_no: cell_no}
#       ) do
#     Logging.debug("ingress sms :~p arrived", [sms])
#     Logging.debug("~p get service definition for ~p", [cell_no, service_no])

#     rtn =
#       if state.last_10_processed_messages |> Enum.member?(sms.id) do
#         Logging.debug("message: ~p already processed.", [sms.id])
#         {:reply, :trye, state}
#       else
#         script = get_service_script(sms, state)

#         send_to_queue(
#           Dispatcher.Process.VAS.UserProcess.Script.run_script(
#             script,
#             sms,
#             state,
#             state.cel_script_limits.run_timeout
#           ),
#           sms,
#           state
#         )

#         state = %State{
#           state
#           | last_arrived_message_time: Utilities.now()
#         }

#         state = update_last_10_processed_messages(state, sms)
#         {:reply, true, state}
#       end

#     Logging.debug("log_event called")

#     EventLogger.log_event(
#       ModelUtils.get_entity_type(sms),
#       ModelUtils.get_entity_id(sms),
#       "process",
#       %{}
#     )

#     rtn
#   end

#   def handle_call(
#         msg,
#         _,
#         state = %State{cell_no: cell_no}
#       ) do
#     Logging.debug("~p: got msg:~p but have no plan to do what. ignore it", [cell_no, msg])
#     state = %State{state | last_arrived_message_time: Utilities.now()}
#     {:reply, :ok, state}
#   end

#   #
#   #  @compile {:inline, get_script_run_timeout: 2}
#   #  defp get_script_run_timeout(_sms, state) do
#   #    state.cel_script_limits.run_timeout
#   #  end

#   @compile {:inline, get_service_script: 2}
#   defp get_service_script(%DatabaseEngine.Models.SMS{receiver: "3285"}, _state) do
#     """
#       print ("service masoud is called.")
#       sms = cel.incoming_message
#       function is_member(sender)
#         key = "3285" .. _G.cel.incoming_message.sender
#         value = "3285"
#         return _G.cel.kvdb.get(key) == "3285"
#       end

#       function join()
#         key = "3285" .. _G.cel.incoming_message.sender
#         value = "3285"
#         _G.cel.kvdb.set(key,value)
#       end

#       you_are_member_already = "you are servic member, ask time or date or send 0 to unsub"

#       if is_member() then
#         if sms.body == "1" then
#           cel.reply(you_are_member_already)
#         end

#       else
#         if sms.body == "1" then
#           join()
#           cel.reply("Welcome to msd service. ask time or date or send 0 to unsub")
#         end

#       end
#     """
#   end

#   defp get_service_script(_sms, _state) do
#     """
#       print("hello world")
#       print("message sender is: ",cel.incoming_message.sender)
#       cel.log("debug","hello, this is a kafka enqueue call for script")
#       sms = cel.incoming_message
#       if sms.body == "salam" then
#         cel.reply("aleyk salam")
#       else
#         cel.reply("Unknown")
#       end
#     """
#   end

#   #  defp get_service_definition(_msg) do
#   #  end

#   def handle_cast(_msg, state) do
#     {:noreply, state}
#   end

#   def handle_info(
#         :timeout,
#         state = %State{last_arrived_message_time: last_arrived_message_time, cell_no: cell_no}
#       ) do
#     Logging.debug("~p :timeout called. last_arrived_message_time:~p", [
#       cell_no,
#       last_arrived_message_time
#     ])

#     if Utilities.now() - last_arrived_message_time > @wait_to_new_message_timeout_to_hibernate do
#       Logging.debug("~p Enter in Hibernate Mode. Current State:~p", [cell_no, state])
#       :proc_lib.hibernate(:gen_server, :enter_loop, [__MODULE__, [], state])
#     end

#     Logging.debug("~p All thing is right, let try hibernating later.", [cell_no])
#     Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)
#     {:noreply, state}
#   end

#   def handle_info(
#         @check_last_arrived_message_time,
#         state = %State{last_arrived_message_time: last_arrived_message_time, cell_no: cell_no}
#       ) do
#     Logging.debug(
#       "~p Called. msg:~p With Params Last Arrived Message:~p",
#       [cell_no, @check_last_arrived_message_time, last_arrived_message_time]
#     )

#     if Utilities.now() - last_arrived_message_time >= @wait_to_new_message_timeout_to_terminate do
#       Logging.debug("~p: Stop process:~p cause of no message after long time, So let stop it", [
#         cell_no,
#         self()
#       ])

#       Process.exit(self(), :normal)
#     end

#     Process.send_after(
#       self(),
#       @check_last_arrived_message_time,
#       @wait_to_new_message_timeout_to_terminate + 100
#     )

#     {:noreply, state}
#   end

#   @compile {:inline, watchdog: 0}

#   defp watchdog() do
#     Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)

#     Process.send_after(
#       self(),
#       @check_last_arrived_message_time,
#       @wait_to_new_message_timeout_to_terminate + 100
#     )
#   end
# end
