defmodule ProcessManager.Script do
  require Logger
  require Utilities.Logging
  require GatewayCore.Utils.Helper

  alias Utilities.Logging
  alias :luerl, as: LUA

  @orig_message_key :orig_message_key
  @spec run_script(String.t(), any(), any(), integer()) ::
          {:return, any()} | {:error, any()}
  @spec run_script(any(), any(), any(), any(), :infinity | non_neg_integer()) :: {any(), any()}
  def parse_rtn_value(r, lstate) do
    r
    |> Enum.map(fn x ->
      case x do
        {:tref, _} -> LUA.decode(x, lstate)
        _ -> x
      end
    end)
    |> ProcessManager.Script.Utilities.to_elixir()
  end

  def run_script(
        script_to_run,
        msg,
        user_process_state,
        additional_functionality \\ %{},
        script_run_timeout \\ 5000
      ) do
    Logging.debug(
      "Called. script_to_run:~p msg:~p user_process_state:~p additional_functionalities:~p timeout:~p",
      [script_to_run, msg, user_process_state, additional_functionality, script_run_timeout]
    )

    main_process_id = self()
    ref = make_ref()

    pid =
      Process.spawn(
        fn ->
          try do
            lua_state = init_lua(msg, additional_functionality)

            set_orig_message(msg, lua_state)
            set_user_process_state(user_process_state)

            {r, lstate} = LUA.do(script_to_run, lua_state)

            send(main_process_id, {:script, ref, :return, parse_rtn_value(r, lstate)})
          rescue
            e ->
              Logging.debug("Exception happen it is :~p", [e])
              send(main_process_id, {:script, ref, :error, e})
          end
        end,
        priority: :low
      )

    receive do
      {:script, m, status, r} when m == ref ->
        Logging.debug("script returned with state:~p and result:~p", [status, r])
        {status, r}
    after
      script_run_timeout ->
        Logging.debug("script timeouted then kill it")
        Process.exit(pid, :kill)
        {:error, "timeout"}
    end
  end

  @compile {:inline, get_orig_message: 1}
  defp get_orig_message(_state) do
    Process.get(@orig_message_key)
  end

  @compile {:inline, set_orig_message: 2}
  defp set_orig_message(msg, _state) do
    Process.put(@orig_message_key, msg)
  end

  @compile {:inline, set_user_process_state: 1}
  defp set_user_process_state(state) do
    Process.put(:user_process_state, state)
  end

  @compile {:inline, get_user_process_state: 0}
  defp get_user_process_state() do
    Process.get(:user_process_state)
  end

  def in_array(args, state) do
    Logging.debug("in_array_called with args:~p~n", [args])
    [item, list] = args

    result =
      list
      |> Enum.map(fn {_, x} -> x end)
      |> Enum.find(fn x -> x == item end) != nil

    {[result], state}
  end

  def logging(args, state) do
    Logging.debug("cel logging called with args:~p", [args])
    ups = get_user_process_state()
    Logging.debug("User Process State:~p", [ups])
    logging_queue = Kernel.get_in(ups.queues, [:cel_logging_Q])

    case logging_queue do
      nil ->
        Logging.debug("No Cel Logging Queue Found, ignore logging")

      _ ->
        [level, msg] = args
        Logging.debug("level:~p , msg:~p", [level, msg])

        data_to_enqueue = %{
          "level" => level,
          "message" => msg,
          "type" => "cel.logging",
          "time" => Utilities.now(),
          "module" => __MODULE__
        }

        Logging.debug("data_to_enqueue:~p", [data_to_enqueue])

        case DatabaseEngine.DurableQueue.enqueue(logging_queue, data_to_enqueue) do
          :nok ->
            Logging.warn("Problem to enqueue cel logging. data:~p", data_to_enqueue)

          _ ->
            Logging.debug("Successfully Enqueued.")
            :ok
        end
    end

    {[true], state}
  end

  def is_subscribed(_args, state) do
    {[true], state}
  end

  def unsubscribe(_args, state) do
    :io.format("unsubscribe called in erl~n")
    {[true], state}
  end

  def reply(args, state) do
    Logging.debug("reply called. args:~p~n", [args])
    {[true], state}
  end

  defp init_lua(msg, additional_functionality \\ %{}) do
    s0 = LUA.init()

    incoming_message =
      Utilities.Conversion.nested_map_to_tuple_list(msg)
      |> Utilities.nested_tuple_to_list
      |> Utilities.for_each_non_iterable_item(fn x ->
        case x do
          v when is_pid(v) -> :erlang.pid_to_list(v)
          v -> v
        end
      end)
    Logging.debug("in_msg: ~p converted to: ~p",[msg,incoming_message])

    fns = [
      ProcessManager.Script.Functionalities.HTTP.lua_functionalities(),
      ProcessManager.Script.Functionalities.KVDB.lua_functionalities(),
      ProcessManager.Script.Functionalities.Radius.lua_functionalities()
    ]

    fns_map =
      fns
      |> Enum.reduce(%{}, fn i, acc ->
        Map.merge(acc, i)
      end)

    fns_map = Map.merge(fns_map, additional_functionality)

    functionalities =
      Map.merge(
        fns_map,
        %{
          "incoming_message" => incoming_message,
          "is_subscribed" => &is_subscribed/2,
          "unsubscribe" => &unsubscribe/2,
          "in_array" => &in_array/2,
          "reply" => &reply/2,
          "log" => &logging/2,
          "service" =>
            Map.to_list(%{
              "unsub_keys" => ["a", "13", "43", "1234"]
            })
        }
      )

    # Logging.debug("funcs:~p",[functionalities |> Map.to_list])

    s1 =
      LUA.set_table(
        [:cel],
        Map.to_list(functionalities),
        s0
      )

    s1
  end
end
