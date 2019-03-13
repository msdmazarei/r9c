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
          "transaction" => &kvdb_transaction/2
        })
    }
  end

  def kvdb_transaction(args,state) do
    Logging.debug("called. args:~p",[args])
    f= args[:function]
    rtn = f.([])
    #run passed function
    {rtn,state}
  end
  def kvdb_get(args, state) do
    Logging.debug("kvdb_get called args:~p~n", [args])

    [key | _] = args
    result = DatabaseEngine.Interface.KV.get(key)
    Logging.debug("kvdb_get return: ~p", result)
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
