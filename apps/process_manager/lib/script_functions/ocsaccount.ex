defmodule ProcessManager.Script.Functionalities.OCSAccount do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.OCSAccount
  alias DatabaseEngine.Interface.OCSAccount, as: DB

  def lua_functionalities() do
    %{
      "ocs_account" =>
        Map.to_list(%{
          "get" => &ocsaccount_get/2,
          "set" => &ocsaccount_set/2,
          "get_for_update" => &ocsaccount_get_with_write_lock/2,
          "transaction" => &ocsaccount_transaction/2
        })
    }
  end

  def ocsaccount_transaction(args, state) do
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

  @spec ocsaccount_get(nonempty_maybe_improper_list(), any()) :: {[...], any()}
  def ocsaccount_get(args, state) do
    # Logging.debug("ocsaccount_get called args:~p~n", [args])

    [key | _] = args
    result = DB.get(key)
    # Logging.debug("ocsaccount_get return: ~p", [result])
    {[result], state}
  end

  def ocsaccount_get_with_write_lock(args, state) do
    # Logging.debug("Called. args:~p", [args])
    [key | _] = args
    result = DB.get_for_update(key)
    # Logging.debug("ocsaccount_get_for_update return :~p", [result])
    {[result], state}
  end

  def ocsaccount_set(args, state) do
    [k, v] = args
    # Logging.debug("ocsaccount_set called args:~p~n", [args])
    result = DB.set(k, v)
    # Logging.debug("ocsaccount_set return:~p", [result])
    {[result], state}
  end
end
