defmodule Utilities.Conversion do
  @moduledoc false
  require Utilities

  def replace_all_bins_to_list(obj) do
    Utilities.for_each_non_iterable_item(obj, fn x ->
      if is_binary(x) do
        :binary.bin_to_list(x)
      else
        x
      end
    end)
  end

  def tuple_list_to_map(tp = [{_, _} | _]) when is_list(tp) do
    tp
    |> Enum.map(fn {k, v} ->
      if is_list(v) do
        {k, tuple_list_to_map(v)}
      else
        {k, v}
      end
    end)
    |> Map.new()
  end

  def tuple_list_to_map(tp) when is_list(tp) do
    tp
    |> Enum.map(fn x ->
      case x do
        [{_, _} | _] -> tuple_list_to_map(x)
        r when is_list(r) -> tuple_list_to_map(r)
        _ -> x
      end
    end)
  end

  def nested_map_to_tuple_list(l) when is_list(l) do
    l
    |> Enum.map(fn x ->
      is_list_map = {is_list(x), is_map(x)}

      case is_list_map do
        {true, _} -> nested_map_to_tuple_list(x)
        {_, true} -> nested_map_to_tuple_list(x)
        {false, false} -> x
      end
    end)
  end

  def nested_map_to_tuple_list(map) when is_map(map) do
    Map.to_list(map)
    |> Enum.map(fn {k, v} ->
      is_list_map = {is_list(v), is_map(v)}

      r =
        case is_list_map do
          {true, _} -> nested_map_to_tuple_list(v)
          {_, true} -> nested_map_to_tuple_list(v)
          {false, false} -> v
        end

      {k, r}
    end)
  end
end
