defmodule Dispatcher.Process.ProcessModel do
  defstruct name: nil, node_address: nil, local_pid: nil, start_time: nil
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

  @spec start_local_gen_server(atom(), any(), [
          {:debug, [any()]}
          | {:hibernate_after, :infinity | non_neg_integer()}
          | {:name, atom() | {any(), any()} | {any(), any(), any()}}
          | {:spawn_opt, :link | :monitor | {any(), any()}}
          | {:timeout, :infinity | non_neg_integer()}
        ]) :: false | pid()
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
    Dispatcher.Protocols.DispatcherInfo.get_unit_process_module_for_message(message)
  end

  def get_process_name_by_message(message) do
    Dispatcher.Protocols.DispatcherInfo.get_process_name(message)

    # case message do
    #   %DatabaseEngine.Models.SMS{
    #     sender: receiver
    #   } ->
    #     receiver

    #   %DatabaseEngine.Models.RadiusPacket{
    #     attribs: attrs
    #   } ->
    #     username_attr_id = 1
    #     acct_session_id = 44
    #     acct_multi_session_id = 50

    #     r =
    #       attrs[username_attr_id] || attrs[acct_session_id] ||
    #         attrs[acct_multi_session_id]

    #     r
    # end
  end

  def send_messages(messages) do
    process_names =
      messages
      |> Enum.map(&get_process_name_by_message/1)

    process_message_tuples = Enum.zip(process_names, messages)

    results =
      process_message_tuples
      |> Enum.map(&send_message/1)

    wait_to_response_or_timeout(
      Utilities.now(),
      results,
      process_message_tuples
    )
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

  def check_user_process_result(process_result, process_message_tuple = {pname, msg})
      when is_pid(process_result) do
    Logging.debug("PARSE PID X TO FIND OUT RESULT:~p", [process_result])
    x = process_result

    case :rpc.nb_yield(x, 0) do
      :timeout ->
        {x, process_message_tuple}

      {:value, v} ->
        case v do
          r when is_boolean(r) ->
            {r, process_message_tuple}

          {:badrpc, {:EXIT, {:noproc, _}}} ->
            Logging.debug("EXITED PROCESS!!!")

            Logging.debug("there is no process for pname:~p", [pname])

            {:atomic, _} =
              Mnesia.transaction(fn ->
                DatabaseEngine.Interface.Process.del(pname)
                #                            Mnesia.dirty_delete({@table_name, pname})
              end)

            new_message = could_send_to_process_again(msg)

            case new_message do
              nil ->
                Logging.debug("should wait beacuse it is soon to send message again")
                {x, process_message_tuple}

              _ ->
                Logging.debug("send message again, new_message options:~p", [new_message.options])
                new_pid = send_message({pname, new_message})
                {new_pid, {pname, new_message}}
            end

          _ ->
            Logging.warn(
              "RPC VALUE is :~p (we expect boolean or badrpc. we consider it as false)",
              [v]
            )

            {false, process_message_tuple}
        end
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

  @spec send_message({String.t(), DatabaseEngine.Models.SMS}) :: pid() | boolean()
  def send_message(args = {process_name, message}) do
    Logging.debug("Called With args:~p", [args])

    tran_res =
      Mnesia.transaction(fn ->
        #          case Mnesia.read({@table_name, process_name}) do
        result =
          case DatabaseEngine.Interface.Process.get(process_name) do
            {:aborted, reason} ->
              Logging.error("could not get_process_model cause of :~p", [reason])
              false

            v when v in [[], nil] ->
              process_creation_result = create_process(process_name, message)
              Logging.debug("Process creation result:~p", [process_creation_result])
              process_creation_result

            process_model = %Dispatcher.Process.ProcessModel{} ->
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
                [pid, {:ingress, message}, @user_process_timeout]
              )

            Logging.debug("async_call result:~p", [r])
            r
        end
      end)

    case tran_res do
      {:atomic, v} ->
        Logging.debug("Transaction Done With Return Value:~p", [v])

        EventLogger.log_event(
          ModelUtils.get_entity_type(message),
          ModelUtils.get_entity_id(message),
          "SEND_MSG",
          %{"process_name" => process_name}
        )

        v

      {:aborted, r} ->
        Logging.debug("Transaction Failed With Return Value:~p", [r])

        EventLogger.log_event(
          ModelUtils.get_entity_type(message),
          ModelUtils.get_entity_id(message),
          "SEND_MSG_FAILED",
          %{"process_name" => process_name}
        )

        r
    end
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

  def create_process(name, message) do
    create_process(
      name,
      get_module_by_message(message),
      Utilities.all_user_process_nodes(),
      message
    )
  end

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
           [module, %{"cell_no" => name}],
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
