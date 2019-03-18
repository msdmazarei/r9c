defmodule ProcessManager.Script.Functionalities.Utils do
  use Bitwise
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require ProcessManager.Script.Utilities
  alias ProcessManager.Script.Utilities, as: Script
  @lua_module_path Application.get_env(:process_manager, ProcessManager.Script)[:lua_modules_path]

  def lua_functionalities() do
    %{
      "utils" =>
        Map.to_list(%{
          "sleep" => &sleep/2,
          "unixepoch_now" => &now/2,
          "debug_print" => &debug_print/2,
          "compile_code" => &compile_code/2,
          "call_function_in_state" => &call_function_in_state/2
        })
    }
  end

  def debug_print(args, state) do
    Logging.debug("args:~p", [args])
    {[true], state}
  end

  def sleep(args, state) do
    Logging.debug("Called.")

    wait_time =
      case args do
        [w] when is_number(w) ->
          Kernel.trunc(w)

        _ ->
          Logging.debug("Bad arguments to sleep. it accepts number only. you passed:~p", [args])
          throw("bad argument")
      end

    :timer.sleep(Kernel.trunc(wait_time))

    {[true], state}
  end

  def now(args, state) do
    # to make it will be float

    {[Utilities.now() + 0.1], state}
  end

  def compile_code(args, state) do
    Logging.debug("called.")
    [code] = args
    code = "package.path = package.path .. \";" <> (@lua_module_path || "") <> "\"\n" <> code
    Logging.debug("code to compile:~p", [code])

    rtn = try do
      {_result, st} = :luerl.do(code)
      Logging.debug("Compiled")
      Logging.debug("converting state to bin")
      serialized_state = Utilities.Serializers.BinSerializer.serialize(st)

      {[serialized_state], state}
    rescue
      x ->
        Logging.debug("error happend in compiling code. error:~p", [x])
        {[false], state}
    end
    rtn
  end

  def call_function_in_state(args, state) do
    try do
      [funcname, funcargs, st] = args
      Logging.debug("funcargs is:~p",[funcargs])
      st = Utilities.Serializers.BinSerializer.deserialize(st)
      funcargs = Script.to_elixir(funcargs) |> Enum.map(
        fn x ->
          Script.to_lua(x)
        end
      )
      Logging.debug("call function with args:~p",[funcargs])
      {[rtn|_], _} = :luerl.call_function([funcname], funcargs, st)
      Logging.debug("it returned ~p",[rtn])
      {[true, rtn], state}
    rescue
      x ->
        Logging.debug("error in execute function in state:~p", [x])
        {[false, "error"], state}
    end
  end
end
