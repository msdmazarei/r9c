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
          "transaction" => &kvdb_transaction/2,
          "kvdb_func_pass" => &kvdb_func_pass/2
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
               [success_status, frtn] = f.(func_arguments)

               if success_status == false do
                 Logging.debug("rolling back transaction")
                 :mnesia.abort(frtn)
               else
                 Logging.debug("committing transaction")
                 frtn
               end
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

  @spec kvdb_get(nonempty_maybe_improper_list(), any()) :: {any(), any()}
  def kvdb_get(args, state) do
    Logging.debug("kvdb_get called args:~p~n", [args])

    [key | _] = args
    result = DatabaseEngine.Interface.KV.get(key)
    Logging.debug("kvdb_get return: ~p", [result])

    result = ProcessManager.Script.Utilities.to_lua(result)

    # Utilities.iter_over_all_iterables(
    #     result,
    #     fn item ->
    #       case item do
    #         {:function, f} ->
    #           fn a, s ->
    #             {[f.(a)], s}
    #           end

    #         _ ->
    #           item
    #       end
    #     end,
    #     false
    #   )

    st = state
    # {result, st } =
    #   case result do
    #     {:function, f1} ->

    #      fp = fn ars, st ->
    #       Logging.debug("Called. internal function--------")
    #       {f1.(ars),st}
    #      end
    #      {[fp],state}
    #     #  {fn_no , _} = :luerl.get_table([:msd_dynfun_counter], state)
    #     #  fn_no  = (fn_no || 1) +1

    #     #  fn_name = "fn_#{fn_no}"
    #     #  Logging.debug("fn_name: ~p",[fn_name])
    #     #  nstate = :luerl.set_table([:msd_dynfun_counter], fn_no, state)
    #     #  nstate = :luerl.set_table([fn_name], fp, nstate)
    #     #  {res,_ } = :luerl.do(
    #     #    "return #{fn_name}", nstate
    #     #  )
    #     #  Logging.debug("result:~p",[res])
    #     #  {[fn_name],nstate}

    #     v ->
    #       {[v], state}
    #   end

    Logging.debug("reuslt:~p", [result])

    {[result], st}
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

  def kvdb_func_pass(args, state) do
    f = fn as, st ->
      :io.fwrite("simple function to pass")
      {[true], st}
    end

    {[f], state}
  end
end
