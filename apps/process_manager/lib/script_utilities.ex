defmodule ProcessManager.Script.Utilities do
  def to_lua(var) when is_number(var) or is_binary(var) do
    var
  end

  def to_lua(var) do
    v1 = Utilities.nested_tuple_to_list(var)
    v2 = Utilities.Conversion.nested_map_to_tuple_list(v1)
    v2
  end

  def to_elixir(var) when is_list(var) do
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
  end

  def to_elixir(var) when is_number(var) or is_bitstring(var) do
    var
  end
end
