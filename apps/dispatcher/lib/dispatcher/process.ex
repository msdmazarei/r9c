defmodule Dispatcher.Process.ProcessModel do
  defstruct name: nil, node_address: nil, local_pid: nil, start_time: nil
end

defmodule Dispatcher.Process.RemoteTasks do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

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

  def start_bunch_of_process_locally(process_message_tuple_list) do
    Logging.debug("called. node:~p - process_message count:~p", [
      node(),
      process_message_tuple_list |> length
    ])

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
  end
end

# :mnesia.dirty_delete(Dispatcher.Process.ProcessModel, "989360076133"
defmodule Dispatcher.Process do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require DatabaseEngine.Utils.EventLogger
  alias DatabaseEngine.Utils.EventLogger
  alias DatabaseEngine.Models.Utils, as: ModelUtils

  @config Application.get_env(:dispatcher, __MODULE__)
  @process_nodes @config[:process_nodes]
  @alive_response_for_UP @config[:alive_response_for_UP]
  @maximum_wait_time_for_UP_responses @config[:maximum_wait_time_for_UP_responses]
  @user_process_timeout @config[:user_process_timeout]
  @process_creation_timeout @config[:process_creation_timeout]

  alias :mnesia, as: Mnesia

  def get_sample_sms(mobno) do
    %DatabaseEngine.Models.SMS{
      receiver: mobno,
      sender: mobno
    }
  end

  def hello_world() do
    Logging.debug("hello world.")
  end

  def get_process_name_by_message(message) do
    ProcessManager.UnitProcess.Identifier.get_process_name(message)
  end

  def generate_processes(process_message_tuple_list) do
    Logging.debug("Called. process_message count:~p", [length(process_message_tuple_list)])

    nodes =
      Utilities.all_active_nodes()
      |> Enum.filter(fn n ->
        @process_nodes |> Enum.member?(to_string(n))
      end)
      |> Enum.shuffle()

    case nodes do
      [] ->
        Logging.error("no process nodes found to dispatch. actives:~p allowed list:~p", [
          Utilities.all_active_nodes(),
          @process_nodes
        ])

      _ ->
        node_count = length(nodes)
        process_count_per_node = Kernel.ceil(length(process_message_tuple_list) / node_count)

        start_indices =
          :lists.seq(0, node_count - 1)
          |> Enum.map(fn x ->
            x * process_count_per_node
          end)

        slices =
          start_indices
          |> Enum.map(fn s ->
            Enum.slice(process_message_tuple_list, s, process_count_per_node)
          end)

        node_slice = Enum.zip(nodes, slices)

        Logging.debug("node_slice:~p", [
          node_slice
          |> Enum.map(fn {n, lst} ->
            {n, lst |> Enum.map(fn {pn, _} -> pn end)}
          end)
        ])

        pids =
          node_slice
          |> Enum.map(fn {nodename, process_msg_tuple_list} ->
            Logging.debug("nodename:~p", [nodename])

            :erlang.spawn(
              nodename,
              Dispatcher.Process.RemoteTasks,
              :remote_agent_to_create_process_locally,
              [process_msg_tuple_list]
            )
          end)

        waiter(pids, Utilities.now() + @process_creation_timeout)
    end
  end

  def waiter(pids, wait_till_time \\ :infinity) do
    # Logging.debug("Called. pid length:~p", [pids |> length])

    should_wait =
      pids
      |> Enum.map(fn pid ->
        case :rpc.block_call(pid |> node(), :erlang, :is_process_alive, [pid], 1_000) do
          {:badrpc, reason} ->
            Logging.debug("badrpc to get live status of pocess:~p reason:~p", [pid, reason])
            false

          v ->
            v
        end
      end)
      |> Enum.reduce(false, fn r, acc ->
        acc or r
      end)

    case should_wait do
      true ->
        expired =
          if wait_till_time == :infinity do
            false
          else
            Utilities.now() > wait_till_time
          end

        if expired do
          :ok
        else
          waiter(pids)
        end

      false ->
        :ok
    end
  end

  def send_messages(messages) do
    Logging.debug("called.")
    Logging.debug("called with messages length:~p", [messages |> length])

    process_names =
      messages
      |> Enum.map(&get_process_name_by_message/1)

    # remove process_names which are nil
    ignored_message_count =
      process_names
      |> Enum.filter(fn x ->
        case x do
          nil -> true
          _ -> false
        end
      end)
      |> length()

    Logging.debug("ignored message (nil process_name) count:~p", [ignored_message_count])

    process_message_tuples =
      Enum.zip(process_names, messages)
      |> Enum.filter(fn {pn, _} ->
        case pn do
          nil -> false
          _ -> true
        end
      end)

    not_exist_processes_process_messgae_tuple =
      process_message_tuples
      |> Enum.filter(fn {pn, _} ->
        DatabaseEngine.Interface.Process.get(pn) == nil
      end)

    st = Utilities.now()
    generate_processes(not_exist_processes_process_messgae_tuple)
    process_creation_time = Utilities.now() - st

    st = Utilities.now()
    not_delivered_process_message_tuple_list = dispatch_messages_to_nodes(process_message_tuples)
    send_message_to_process_time = Utilities.now() - st

    need_to_requeue =
      not_delivered_process_message_tuple_list
      |> Enum.map(fn {pn, msg} ->
        DatabaseEngine.Interface.Process.del(pn)
        msg
      end)

    %{
      "stats" => %{
        "send_message_to_process_time" => send_message_to_process_time,
        "process_creation_time" => process_creation_time
      },
      "need_to_requeue" => need_to_requeue
    }
  end

  def ok_nok_to_boolean(v) do
    case v do
      :ok -> true
      :nok -> false
      _ -> v
    end
  end

  def could_send_to_process_again(msg = %{options: options}) do
    Logging.debug("Called")
    key_time = "dispatcher_last_send_to_process_time"
    key_try_count = "dispatcher_total_send_try_count"
    new_options = options || %{}

    time = new_options[key_time] || 0
    try_count = new_options[key_try_count] || 0
    total_try = 4
    delay_time = @maximum_wait_time_for_UP_responses / total_try
    Logging.debug("time:~p try_count:~p", [time, try_count])

    if time > Utilities.now() or try_count > total_try do
      Logging.debug("return nil")
      nil
    else
      new_options =
        new_options
        |> Map.put(key_time, Utilities.now() + 1000)
        |> Map.put(key_try_count, try_count + 1)

      r = %{msg | options: new_options}
      Logging.debug("returns:~p", [r])
      r
    end
  end

  def check_user_process_result(process_result, process_message_tuple) do
    Logging.debug("process_result is :~p", [process_result])
    {process_result, process_message_tuple}
  end

  @spec wait_to_response_or_timeout(integer(), list(), list()) :: list(boolean())
  def wait_to_response_or_timeout(start_time, final_result, process_message_tuples) do
    Logging.debug("Called With Params, start_time:~p final-result:~p", [start_time, final_result])

    all_of_items_are_boolean =
      final_result
      |> Enum.reduce(true, fn x, acc -> acc && is_boolean(x) end)

    case all_of_items_are_boolean do
      true ->
        send_failed_dispatched_to_kafka(final_result, process_message_tuples)
        final_result

      false ->
        if Utilities.now() < start_time + @maximum_wait_time_for_UP_responses do
          new_ =
            final_result
            |> Enum.map(&ok_nok_to_boolean/1)
            |> Enum.with_index()
            |> Enum.map(fn {x, index} ->
              process_message_tuple = process_message_tuples |> Enum.at(index)
              check_user_process_result(x, process_message_tuple)
            end)

          new_results = new_ |> Enum.map(fn {x, _} -> x end)
          new_process_message_tuples = new_ |> Enum.map(fn {_, x} -> x end)
          Process.sleep(500)

          Logging.debug("will call with results:~p process_message_tuple:~p", [
            new_results,
            new_process_message_tuples
          ])

          wait_to_response_or_timeout(start_time, new_results, new_process_message_tuples)
        else
          # timeouted
          final_result =
            final_result
            |> Enum.map(&ok_nok_to_boolean/1)
            |> Enum.map(fn x ->
              if is_boolean(x) do
                x
              else
                false
              end
            end)

          send_failed_dispatched_to_kafka(final_result, process_message_tuples)

          final_result
        end
    end
  end

  @dispatcher_fail_Q @config[:dispatcher_fail_Q]

  defp send_failed_dispatched_to_kafka(final_result, process_message_tuples) do
    Logging.debug("Called. final result:~p", [final_result])

    final_result
    |> Enum.with_index()
    |> Enum.map(fn {item, index} ->
      if item == false do
        process_message_tuples
        |> Enum.at(index)
      else
        nil
      end
    end)
    |> Enum.filter(fn x -> x != nil end)
    |> Enum.map(fn {pname, msg} ->
      msg =
        case msg do
          %DatabaseEngine.Models.SMS{} ->
            DatabaseEngine.Models.SMS.Helper.describe_stage(
              msg,
              "dispatcher",
              "failed_to_dispatch"
            )

          _ ->
            msg
        end

      case DatabaseEngine.DurableQueue.enqueue(@dispatcher_fail_Q, msg) do
        :nok ->
          Logging.warn("Problem To Enqueue messages to dispatcher fail Q. for pname:~p", [pname])

        _ ->
          Logging.debug(
            "successfully enqueued to dispatcher failed Q, process name :~p",
            [
              pname
            ]
          )

          :ok
      end
    end)
  end

  def wait_for_refs(till_time, refs) when is_map(refs) do
    # some times directely read from inbox is better than using receive
    # Logging.debug("node:~p refs:~p", [node, refs |> Map.keys()])

    if Utilities.now() < till_time do
      new_ref =
        receive do
          in_msg ->
            case in_msg do
              r when is_reference(r) ->
                Logging.debug("node:~p - a ref arrived from a process. ref:~p", [
                  node(),
                  r
                ])

                refs |> Map.delete(r)

              _ ->
                refs
            end
        after
          1 ->
            refs
        end

      if new_ref |> Map.keys() |> length == 0 do
        %{}
      else
        wait_for_refs(till_time, new_ref)
      end
    else
      refs
    end
  end

  def send_bunch_of_messages_locally(process_message_tuple_list) do
    # 1. create refrence per each message
    # 2. send message to procrss name and knowing which message is seent and which one has no process
    # 3. wait to get ack from user process for 1second

    Logging.debug("called on node:~p", [node()])
    refs = process_message_tuple_list |> Enum.map(fn _ -> make_ref() end)
    map_ref_pnm = Enum.zip(refs, process_message_tuple_list) |> Map.new()
    Logging.debug("running tasks")

    task =
      Task.async(fn ->
        send_result_refs =
          process_message_tuple_list
          |> Enum.with_index()
          |> Enum.map(fn {{pn, msg}, indx} ->
            pmodel = DatabaseEngine.Interface.Process.get(pn)

            ref = refs |> Enum.at(indx)

            if pmodel == nil do
              {nil, ref}
            else
              send(pmodel.local_pid, {:ingress, msg, self(), ref})
              {:ok, ref}
            end
          end)

        Logging.debug("node:~p all messages sent", [node()])

        no_have_process_refs =
          send_result_refs
          |> Enum.filter(fn {a, _} ->
            a == nil
          end)
          |> Enum.map(fn {_, b} -> b end)

        failed_to_send_map =
          no_have_process_refs
          |> Enum.reduce(%{}, fn r, acc ->
            acc |> Map.put(r, map_ref_pnm[r])
          end)

        Logging.debug("no_have_process_refs:~p", [no_have_process_refs])
        success_sent_map = map_ref_pnm |> Map.drop(no_have_process_refs)

        faild_to_get_response = wait_for_refs(Utilities.now() + 1_000, success_sent_map)

        Logging.debug("node:~p - faild_to_get_response:~p", [
          node(),
          faild_to_get_response |> Map.keys()
        ])

        ref_result = Map.merge(faild_to_get_response, failed_to_send_map)
        # will returns process name whick did not send ack back.
        rtn = ref_result |> Map.to_list() |> Enum.map(fn {_, pm} -> pm end)

        Logging.debug("node:~p - process-messages which not delivered:~p", [
          node(),
          rtn |> Enum.map(fn {p, _} -> p end)
        ])

        rtn
      end)

    Task.await(task)
  end

  def dispatch_messages_to_nodes(process_message_tuple_list) do
    Logging.debug("Called.")

    node_per_pm =
      process_message_tuple_list
      |> Enum.map(fn {pn, _} ->
        pm = DatabaseEngine.Interface.Process.get(pn)

        if pm != nil do
          pm.node_address
        else
          nil
        end
      end)

    n_p_m = Enum.zip(node_per_pm, process_message_tuple_list)
    grouped_pm_by_node_map = n_p_m |> Enum.group_by(fn {n, _} -> n end, fn {_, pm} -> pm end)

    {no_process_found_in_processes, grouped_pm_by_node_map} =
      grouped_pm_by_node_map |> Map.pop(nil)

    no_process_found_in_processes = no_process_found_in_processes || []

    tasks =
      grouped_pm_by_node_map
      |> Map.to_list()
      |> Enum.map(fn {nodename, pms} ->
        Logging.debug("spawining - called for nodename:~p", [nodename])

        Task.async(fn ->
          Logging.debug("calling node:~p to send locally messages", [nodename])

          case :rpc.block_call(
                 nodename,
                 Dispatcher.Process,
                 :send_bunch_of_messages_locally,
                 [pms],
                 3_000
               ) do
            {:badrpc, reason} ->
              Logging.debug("badrpc:~p", [reason])
              pms

            r when is_list(r) ->
              Logging.debug("failed to deliver pm_tuple_list:~p", [
                r |> Enum.map(fn {p, _} -> p end)
              ])

              r

            other ->
              Logging.debug("we were waiting to list of processname and msgs not other:~p", [
                other
              ])

              pms
          end
        end)
      end)

    failed_to_delivered_process_message_tuple_list =
      tasks
      |> Enum.map(fn t ->
        Task.await(t)
      end)

    rtn =
      [no_process_found_in_processes, failed_to_delivered_process_message_tuple_list]
      |> List.flatten()

    # Logging.debug("return :~p", [rtn])
    rtn
  end

  def get_process_model(_process_name) do
  end

  def is_process_alive(%Dispatcher.Process.ProcessModel{
        node_address: n,
        local_pid: pid,
        name: name
      }) do
    case :rpc.block_call(
           n,
           GenServer,
           :call,
           [pid, :alive, @alive_response_for_UP],
           @alive_response_for_UP
         ) do
      true ->
        true

      other ->
        Logging.debug(
          "did not get proper response from remote process. It;s Response: ~p",
          [
            other
          ]
        )

        {:atomic, _} =
          Mnesia.transaction(fn ->
            Logging.debug("deleting entry in mnesia")
            r = DatabaseEngine.Interface.Process.del(name)
            #          r = Mnesia.delete({@table_name, name})
            Logging.debug("delete status:~p", [r])
          end)

        false
    end
  end

  def kill_process(_name) do
  end

  # def create_process(name, message) do
  #   create_process(
  #     name,
  #     get_module_by_message(message),
  #     Utilities.all_user_process_nodes(),
  #     message
  #   )
  # end

  def create_process(_, _, [], _) do
    Logging.debug("no nodes passed")
    false
  end

  def create_process(name, module, nodes, _message) do
    Logging.debug("Called With params, name:~p module:~p nodes:~p", [name, module, nodes])

    selected_node =
      nodes
      |> Enum.shuffle()
      |> hd()

    case :rpc.block_call(
           selected_node,
           Dispatcher.Process,
           :start_local_gen_server,
           [module, %{"process_name" => name}],
           @process_creation_timeout
         ) do
      {:badrpc, reason} ->
        Logging.debug("BadRPC:~p", [reason])
        false

      pid when is_pid(pid) ->
        Logging.debug("process created on remote node with pid:~p", [pid])

        process_model = %Dispatcher.Process.ProcessModel{
          node_address: selected_node,
          name: name,
          local_pid: pid,
          start_time: Utilities.now()
        }

        #        case Mnesia.write({@table_name, name, process_model}) do
        case DatabaseEngine.Interface.Process.set(name, process_model) do
          :ok ->
            Logging.debug("process_model:~p stored.", [process_model])
            process_model

          {:aborted, reason} ->
            Logging.debug("transaction abored cause of:~p", [reason])
            false
        end

      other ->
        Logging.debug("Rpc Returned:~p", [other])
        false
    end
  end
end
