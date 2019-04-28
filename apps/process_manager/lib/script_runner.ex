defmodule ProcessManager.Script do
  require Logger
  require Utilities.Logging
  require GatewayCore.Utils.Helper

  alias Utilities.Logging
  alias :luerl, as: LUA
  @lua_module_path Application.get_env(:process_manager, __MODULE__)[:lua_modules_path]
  @orig_message_key :orig_message_key
  @lua_virtual_console :lua_virtual_console
  @spec run_script(String.t(), any(), any(), integer()) ::
          {:return, any()} | {:error, any()}
  @spec run_script(any(), any(), any(), any(), :infinity | non_neg_integer()) :: {any(), any()}
  def parse_rtn_value(r, lstate) do
    Logging.debug("trying to parse returned value:~p", [r])

    parsed_tables =
      r
      |> Enum.map(fn x ->
        case x do
          {:tref, _} -> LUA.decode(x, lstate)
          {:function, _, _, _, _, _} -> LUA.decode(x, lstate)
          _ -> x
        end
      end)

    Logging.debug("to_elixir will call by value:~p", [parsed_tables])

    parsed_tables
    |> ProcessManager.Script.Utilities.to_elixir()
  end

  def run_script(
        script_to_run,
        msg,
        user_process_state,
        additional_functionality \\ %{},
        script_run_timeout \\ 5000,
        using_cache \\ true,
        return_virtual_console \\ false
      ) do
    # Logging.debug(
    #   "Called. script_to_run:~p msg:~p user_process_state:~p additional_functionalities:~p timeout:~p",
    #   [script_to_run, msg, user_process_state, additional_functionality, script_run_timeout]
    # )
    script_to_run =
      "package.path = package.path .. \";" <> (@lua_module_path || "") <> "\"\n" <> script_to_run

    script_hash_value = :crypto.hash(:md5, script_to_run) |> Base.encode16(case: :lower)
    script_key = "script_cached_#{script_hash_value}"

    {compiled_script, lua_state} =
      if using_cache do
        case DatabaseEngine.Interface.LKV.get(script_key) do
          nil ->
            Logging.debug("no script found in cache")
            {nil, nil}

          {till_time, compiled_script, lua_state} ->
            if Utilities.now() > till_time do
              Logging.debug("script found in cache but it is expired")
              DatabaseEngine.Interface.LKV.del(script_key)
              {nil, nil}
            else
              Logging.debug("script found in cache.")
              {compiled_script, lua_state}
            end
        end
      else
        Logging.debug("no cache use for this script")
        {nil, nil}
      end

    Logging.debug("script to run:~p", [script_to_run])
    main_process_id = self()
    ref = make_ref()

    pid =
      Process.spawn(
        fn ->
          try do
            lua_state =
              case lua_state do
                nil ->
                  init_lua(msg, additional_functionality)

                _ ->
                  lua_state
              end

            set_orig_message(msg, lua_state)
            set_user_process_state(user_process_state)

            {compiled_script, lua_state} =
              case compiled_script do
                nil ->
                  Logging.debug("compiling script...")

                  case LUA.load(script_to_run, lua_state) do
                    {:ok, compiled_script, lua_state} ->
                      Logging.debug("compiled successfully. store it in cache for 5 min")

                      DatabaseEngine.Interface.LKV.set(
                        script_key,
                        {Utilities.now() + 300_000, compiled_script, lua_state}
                      )

                      {compiled_script, lua_state}

                    {:error, reason} ->
                      Logging.warn("problem to compile lua script. reason:~p", [reason])
                      {nil, nil}
                  end

                _ ->
                  {compiled_script, lua_state}
              end

            case {compiled_script, lua_state} do
              {nil, nil} ->
                send(main_process_id, {:script, ref, :error, "script not compiled"})

              {a, b} when a != nil and b != nil ->
                Logging.debug("executing the compiled script.")
                {r, lstate} = LUA.do(compiled_script, lua_state)

                send(
                  main_process_id,
                  {:script, ref, :return, parse_rtn_value(r, lstate),
                   Process.get(@lua_virtual_console)}
                )
            end
          rescue
            e ->
              Logging.debug("Exception happen it is :~p", [e])
              send(main_process_id, {:script, ref, :error, e})
          end
        end,
        []
      )

    receive do
      {:script, m, status, r, vc} when m == ref ->
        Logging.debug("script returned with state:~p and result:~p", [status, r])

        if return_virtual_console do
          {status, r, %{"virtual_console" => vc}}
        else
          {status, r}
        end
    after
      script_run_timeout ->
        Logging.debug("script timeouted then kill it")
        Process.exit(pid, :kill)
        {:error, "timeout"}
    end
  end

  @compile {:inline, get_orig_message: 1}
  def get_orig_message(_state) do
    Process.get(@orig_message_key)
  end

  @compile {:inline, set_orig_message: 2}
  def set_orig_message(msg, _state) do
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

  def print(args, state) do
    Logging.debug("called. args:~p", args)
    old = Process.get(@lua_virtual_console) || []
    new = old ++ [args]
    Process.put(@lua_virtual_console, new)
    {[], state}
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
    # configure package.path to retrive packages
    s0 =
      case :os.get_env_var('LUA_LOAD_PATH') do
        false ->
          s0

        v ->
          pathes = Path.join(v, '?.lua') <> ";" <> Path.join(v, '?/init.lua')

          Logging.debug("pathes:~p", [pathes])

          r = :luerl.set_table([:package, :path], pathes, s0)
          Logging.debug("new package.path configed")
          r
      end

    # Logging.debug("path:~p",[:luerl.get_table([:package,:path],s0)])
    incoming_message =
      msg
      |> Utilities.nested_tuple_to_list()
      |> Utilities.for_each_non_iterable_item(fn x ->
        case x do
          v when is_pid(v) -> :erlang.pid_to_list(v)
          v -> v
        end
      end)
      |> Utilities.Conversion.nested_map_to_tuple_list()

    # Logging.debug("in_msg: ~p converted to: ~p",[msg,incoming_message])

    fns = [
      ProcessManager.Script.Functionalities.HTTP.lua_functionalities(),
      ProcessManager.Script.Functionalities.KVDB.lua_functionalities(),
      ProcessManager.Script.Functionalities.Radius.lua_functionalities(),
      ProcessManager.Script.Functionalities.Diameter.lua_functionalities(),
      ProcessManager.Script.Functionalities.Utils.lua_functionalities(),
      ProcessManager.Script.Functionalities.OCSAccount.lua_functionalities()
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
          "print" => &print/2,
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
