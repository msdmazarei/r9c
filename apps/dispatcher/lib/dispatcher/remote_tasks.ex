defmodule Dispatcher.Process.RemoteTasks do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require DatabaseEngine.Utils.EventLogger
  alias DatabaseEngine.Utils.EventLogger

  def remote_agent_to_create_process_locally(process_msg_tuple_list) do
    Logging.debug("spawned for node name:~p", [node()])
    init_pid = :erlang.whereis(:init)
    :erlang.group_leader(init_pid, self())
    start_bunch_of_process_locally(process_msg_tuple_list)
  end

  def get_module_by_message(message) do
    ProcessManager.UnitProcess.Identifier.get_process_module(message)
  end

  def start_local_gen_server(module, args \\ [], options \\ []) do
    Logging.debug("Called With module:~p args:~p options:~p", [module, args, options])

    case GenServer.start(module, args, options) do
      {:ok, pid} ->
        Logging.debug("process created, pid:~p", [pid])
        pid

      {:error, error} ->
        Logging.debug("process creation failed cause of :~p", [error])
        false

      :ignore ->
        Logging.debug("process creation ignored!")
        false
    end
  end

  def stop_local_gen_server(p) do
    Logging.debug("called.")
    GenServer.stop(p)
  end

  def event_log(ename, eid, str_state, s0 \\ "", s1 \\ "", s2 \\ "", s3 \\ "", i0 \\ -1, i1 \\ -1) do
    %{
      "entity_name" => ename,
      "entity_id" => eid,
      "state" => str_state,
      "additional_data" => %{
        "__index" => %{
          "s0" => s0,
          "s1" => s1,
          "s2" => s2,
          "s3" => s3,
          "i0" => i0,
          "i1" => i1
        }
      }
    }
  end

  def start_bunch_of_process_locally(process_message_tuple_list) do
    Logging.debug("called. node:~p - process_message count:~p", [
      node(),
      process_message_tuple_list |> length
    ])

    process_creation_result =
      process_message_tuple_list
      |> Enum.map(fn {pn, msg} ->
        module = get_module_by_message(msg)
        args = %{"process_name" => pn}

        case(start_local_gen_server(module, args, [])) do
          p when is_pid(p) ->
            process_model = %Dispatcher.Process.ProcessModel{
              node_address: node(),
              name: pn,
              local_pid: p,
              start_time: Utilities.now()
            }

            store_result =
              :mnesia.transaction(
                fn ->
                  already_def = DatabaseEngine.Interface.Process.get_for_update(pn)

                  if already_def != nil do
                    :mnesia.abort("already defined")
                  else
                    DatabaseEngine.Interface.Process.set(pn, process_model)
                    DatabaseEngine.Interface.LProcess.set(pn, process_model)
                  end
                end,
                [],
                1
              )

            case store_result do
              {:aborted, err} ->
                Logging.debug("failed to create process:~p", [err])
                stop_local_gen_server(p)

              _ ->
                p
            end

          _ ->
            false
        end
      end)

    events =
      process_message_tuple_list
      |> Enum.with_index()
      |> Enum.map(fn {{pn, msg}, index} ->
        str_e_type = Utilities.get_struct_type(msg) |> Utilities.to_string()

        str_id =
          ProcessManager.UnitProcess.Identifier.get_identifier(msg) |> Utilities.to_string()

        module =
          ProcessManager.UnitProcess.Identifier.get_process_module(msg) |> Utilities.to_string()

        creation_result = process_creation_result |> Enum.at(index) |> Utilities.to_string()
        process_name = pn|>Utilities.to_string()

        event_log(
          str_e_type,
          str_id,
          "process_creating",
          process_name,
          module,
          creation_result
        )
      end)
      Logging.debug("events:~p",[events])

    EventLogger.log_events(events)

    process_creation_result
  end
end
