defmodule ProcessManager.Script.Functionalities.Diameter do
  use Bitwise
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require ProcessManager.Script.Utilities
  alias ProcessManager.Script.Utilities, as: Script

  def lua_functionalities() do
    %{
      "diameter" =>
        Map.to_list(%{
          "test_function" => &test_function/2,
          "avp_value" => &avp_value/2,
          "avp_octet_string" => &avp_octet_string/2
        })
    }
  end

  def test_function(args, state) do
    Logging.debug("Called")
    {[Script.to_lua("test function from elixir")], state}
  end

  def avp_octet_string(args, state) do
    Logging.debug("called.", [])
    [avp] = args

    dia_avp = Script.to_elixir(avp)

    str_val = Utilities.Parsers.Diameter.avp_octet_string_value(dia_avp)
    {[str_val], state}
  end

  def avp_value(args, state) do
    Logging.debug("called.")
    [binval, type] = args
    Logging.debug("called. binval:~p type:~p", [binval, type])

    case type do
      "Unsigned32" ->
        size = 32
        <<val::size(size)>> = binval
        {[Script.to_lua(val)], state}

      "Unsigned64" ->
        size = 64
        <<val::size(size)>> = binval
        {[Script.to_lua(val)], state}
    end
  end
end
