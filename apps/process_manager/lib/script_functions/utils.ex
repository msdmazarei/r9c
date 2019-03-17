defmodule ProcessManager.Script.Functionalities.Utils do
  use Bitwise
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require ProcessManager.Script.Utilities
  alias ProcessManager.Script.Utilities, as: Script

  def lua_functionalities() do
    %{
      "utils" =>
        Map.to_list(%{
          "sleep" => &sleep/2,
          "unixepoch_now" => &now/2,
          "debug_print" => &debug_print/2
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
    #to make it will be float

    {[Utilities.now() + 0.1], state}
  end
end
