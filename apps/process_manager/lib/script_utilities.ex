defmodule ProcessManager.Script.Utilities do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  def to_lua(var) when is_number(var) or is_binary(var) do
    var
  end

  def to_lua(var) do
    v1 = Utilities.nested_tuple_to_list(var)
    v2 = Utilities.Conversion.nested_map_to_tuple_list(v1)
    v2
  end

  def to_elixir(nil) do
    nil
  end
  def to_elixir(var) when is_list(var) do
    r =
      if Utilities.is_list_of_tuples(var) do
        v1 =
          var
          |> Enum.map(fn {k, v} ->
            {k, to_elixir(v)}
          end)

        Map.new(v1)
      else
        var
        |> Enum.map(fn x ->
          to_elixir(x)
        end)
      end

    r
    |> Utilities.for_each_non_iterable_item(fn x ->
      case x do
        v when is_float(v) ->
          if Kernel.trunc(v) == v do
            Kernel.trunc(v)
          else
            v
          end

        v ->
          v
      end
    end)
  end

  def to_elixir(var) when is_number(var) or is_bitstring(var) do
    var
  end
  def to_elixir(any) do
    Logging.debug("called with arg:~p",[any])
    any
  end
end
