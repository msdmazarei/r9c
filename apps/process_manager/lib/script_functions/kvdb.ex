defmodule ProcessManager.Script.Functionalities.KVDB do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.KV

  def lua_functionalities() do
    %{
      "kvdb" =>
        Map.to_list(%{
          "get" => &kvdb_get/2,
          "set" => &kvdb_set/2,
          "get_for_update" => &kvdb_get_with_write_lock/2,
          "transaction" => &kvdb_transaction/2
        })
    }
  end

  def kvdb_transaction(args, state) do
    Logging.debug("called. args:~p", [args])

    {f, func_arguments, retries} =
      case args do
        [{:function, f}, func_arguments, retries]
        when is_list(func_arguments) and
               is_number(retries) ->
          {f, func_arguments, Kernel.trunc(retries)}

        [{:function, f}, func_arguments, "infinity"]
        when is_list(func_arguments) ->
          {f, func_arguments, :infinity}

        _ ->
          Logging.error("passed arguments are not valid")
          throw("passed arguments are not true")
          {0, 0, 0}
      end

    {success, result} =
      case :mnesia.transaction(
             fn ->
               f.(func_arguments)
             end,
             [],
             retries
           ) do
        {:aborted, reason} ->
          {false, reason}

        {:atomic, result} ->
          {true, result}
      end

    Logging.debug("transaction's done with result: ~p", [{success, result}])
    # run passed function
    {[success, result], state}
  end

  def kvdb_get(args, state) do
    Logging.debug("kvdb_get called args:~p~n", [args])

    [key | _] = args
    result = DatabaseEngine.Interface.KV.get(key)
    Logging.debug("kvdb_get return: ~p", result)
    {[result], state}
  end

  def kvdb_get_with_write_lock(args, state) do
    Logging.debug("Called. args:~p", [args])
    [key | _] = args
    result = DatabaseEngine.Interface.KV.get_for_update(key)
    Logging.debug("kvdb_get_for_update return :~p", [result])
    {[result], state}
  end

  def kvdb_set(args, state) do
    [k, v] = args
    Logging.debug("kvdb_set called args:~p~n", [args])
    result = DatabaseEngine.Interface.KV.set(k, v)
    Logging.debug("kvdb_set return:~p", [result])
    {[result], state}
  end
end
