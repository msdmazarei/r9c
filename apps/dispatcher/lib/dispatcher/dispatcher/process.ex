defmodule Dispatcher.Process.ProcessModel do
  defstruct name: nil, node_address: nil, local_pid: nil, start_time: nil
end

# :mnesia.dirty_delete(Dispatcher.Process.ProcessModel, "989360076133"
defmodule Dispatcher.Process do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @alive_response_for_UP 1000
  @maximum_wait_time_for_UP_responses 7000
  @user_process_timeout 5000
  @process_creation_timeout 1000

  @table_name Dispatcher.Process.ProcessModel
  alias :mnesia, as: Mnesia

  def init_mnesia() do
    Mnesia.create_table(@table_name,
      attributes: [:name, :process_model],
      ram_copies: Utilities.all_user_process_nodes()
    )
  end

  def get_sample_sms(mobno) do
    %DatabaseEngine.Models.SMS{
      receiver: mobno,
      sender: mobno
    }
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

  def get_module_by_message(message) do
    case message do
      %DatabaseEngine.Models.SMS{} -> Dispatcher.Process.VAS.UserProcess
    end
  end

  def get_process_name_by_message(message) do
    case message do
      %DatabaseEngine.Models.SMS{
        receiver: receiver
      } ->
        receiver
    end
  end

  def send_messages(messages) do
    process_names = messages |> Enum.map(&get_process_name_by_message/1)
    process_message_tuples = Enum.zip(process_names, messages)
    results = process_message_tuples |> Enum.map(&send_message/1)

    wait_to_response_or_timeout(
      Utilities.now(),
      results,process_message_tuples
    )
  end

  def ok_nok_to_boolean(v) do
    case v do
      :ok -> true
      :nok -> false
      _ -> v
    end
  end

  def wait_to_response_or_timeout(start_time, final_result,process_message_tuples) do
    Logging.debug("Called With Params, start_time:~p final-result:~p", [start_time, final_result])

    all_of_items_are_boolean =
      final_result |> Enum.reduce(true, fn x, acc -> acc && is_boolean(x) end)

    case all_of_items_are_boolean do
      true ->
        final_result

      false ->
        if Utilities.now() < start_time + @maximum_wait_time_for_UP_responses do
          new_results =
            final_result
            |> Enum.map(&ok_nok_to_boolean/1)
            |> Enum.with_index()
            |> Enum.map(fn {x,index} ->
              if is_boolean(x) do
                x
              else
                if is_pid(x) do
                  Logging.debug("PARSE PID X TO FIND OUT RESULT:~p",[x])
                  case :rpc.nb_yield(x, 0) do
                    :timeout ->
                      x

                    {:value, v} ->
                      case v do
                        r when is_boolean(r) -> r
                        {:badrpc,{:'EXIT',{:noproc,_}}} ->
                          Logging.debug("EXITED PROCESS!!!")
                          {pname,msg} = process_message_tuples|> Enum.at(index)
                          Logging.debug("there is no process for pname:~p",[pname])
                          Mnesia.transaction(fn -> Mnesia.dirty_delete({@table_name,pname}) end)
                          send_message({pname,msg})

                          _->
                          Logging.debug("RPC VALUE is :~p",[v])
                            v
                      end

                    other ->
                      Logging.debug("nb_yeild return OTHER:~p", [other])
                      other
                  end
                else
                  false
                end
              end
            end)

          Process.sleep(100)
          wait_to_response_or_timeout(start_time, new_results,process_message_tuples)
        else
          # timeouted
          final_result
          |> Enum.map(&ok_nok_to_boolean/1)
          |> Enum.map(fn x ->
            if is_boolean(x) do
              x
            else
              false
            end
          end)
        end
    end
  end

  def send_message({process_name, message}) do
    tran_res =
      Mnesia.transaction(fn ->
        result =
          case Mnesia.read({@table_name, process_name}) do
            {:aborted, reason} ->
              Logging.error("could not get_process_model cause of :~p", [reason])
              false

            [] ->
              process_creation_result = create_process(process_name, message)
              Logging.debug("Process creation result:~p", [process_creation_result])
              process_creation_result

            [{_, _, process_model}] ->
              Logging.debug("Process Model:~p", [process_model])
              process_model
          end

        case result do
          false ->
            false

          %Dispatcher.Process.ProcessModel{node_address: node_name, local_pid: pid} ->
            Logging.debug("call remote process By GenServer.Call(pid,message,timeout)")

            r =
              :rpc.async_call(
                node_name,
                GenServer,
                :call,
                [pid, message, @user_process_timeout]
              )

            Logging.debug("async_call result:~p", [r])
            r
        end
      end)

    case tran_res do
      {:atomic, v} ->
        Logging.debug("Transaction Done With Return Value:~p", [v])
        v

      {:aborted, r} ->
        Logging.debug("Transaction Failed With Return Value:~p", [r])
        r
    end
  end

  def get_process_model(process_name) do
  end

  def is_process_alive(
        process_model = %Dispatcher.Process.ProcessModel{
          node_address: n,
          local_pid: pid,
          name: name
        }
      ) do
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
        Logging.debug("did not get proper response from remote process. It;s Response: ~p", [
          other
        ])

        Mnesia.transaction(fn ->
          Logging.debug("deleting entry in mnesia")
          r = Mnesia.delete({@table_name, name})
          Logging.debug("delete status:~p", [r])
        end)

        false
    end
  end

  def kill_process(name) do
  end

  def create_process(name, message) do
    create_process(
      name,
      get_module_by_message(message),
      Utilities.all_user_process_nodes()
    )
  end

  def create_process(_, _, []) do
    Logging.debug("no nodes passed")
    false
  end

  def create_process(name, module, nodes) do
    Logging.debug("Called With params, name:~p module:~p nodes:~p", [name, module, nodes])
    selected_node = nodes |> Enum.shuffle() |> hd()

    case :rpc.block_call(
           selected_node,
           Dispatcher.Process,
           :start_local_gen_server,
           [module],
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

        case Mnesia.write({@table_name, name, process_model}) do
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
