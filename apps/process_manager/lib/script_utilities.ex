defmodule ProcessManager.Script.Utilities do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require Utilities

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

  def is_map_lua_list(m) when is_map(m) do
    keys = m |> Map.keys()
    min = keys |> Enum.min()
    max = keys |> Enum.max()
    ukeys = keys |> Enum.uniq()

    if min == 1 and is_number(max) do
      length(ukeys) == max - min + 1
      true
    else
      false
    end
  end

  def is_map_lua_list(_) do
    false
  end

  def to_elixir(var) when is_list(var) do

    r =
      if Utilities.is_list_of_tuples(var) do
        v1 =
          var
          |> Enum.map(fn {k, v} ->
            {k, to_elixir(v)}
          end)

        tmp_ = Map.new(v1)

        tmp_ =
          if tmp_["__struct__"] != nil do
            structed = Utilities.to_struct(String.to_atom(tmp_["__struct__"]), tmp_)
            structed
          else
            tmp_
          end

        tmp_ =
          if is_map_lua_list(tmp_) do
            tmp_ |> Map.values()
          else
            tmp_
          end
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
    Logging.debug("called with arg:~p", [any])
    any
  end
end
