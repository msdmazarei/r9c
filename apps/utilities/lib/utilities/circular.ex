defmodule Utilities.Circular do
  @moduledoc false
  @ciruclar_buf :circular_buffer

  def new_circular_buffer(size) do
    arr = for _ <- 1..size, do: 0
    {@ciruclar_buf, -1, size, arr, %{}}
  end

  def append_to_circular_buffer({@ciruclar_buf, index, size, arr, dict}, value) do
    new_index = rem(index + 1, size)
    old_key = arr |> Enum.at(new_index)
    is_already_member = Enum.member?(arr, value)

    if is_already_member do
      {@ciruclar_buf, index, size, arr, dict |> Map.delete(value)}
    else
      a = List.update_at(arr, new_index, fn _ -> value end)
      nd = dict |> Map.delete(old_key)
      {@ciruclar_buf, new_index, size, a, nd}
    end
  end

  def append_to_circular_buffer({@ciruclar_buf, index, size, arr, dict}, key, value) do
    new_index = rem(index + 1, size)
    old_key = arr |> Enum.at(new_index)
    is_already_member = Enum.member?(arr, key)

    if is_already_member do
      new_dict = dict |> Map.put(key, value)
      {@ciruclar_buf, index, size, arr, new_dict}
    else
      a = List.update_at(arr, new_index, fn _ -> key end)
      nd = dict |> Map.delete(old_key) |> Map.put(key, value)
      {@ciruclar_buf, new_index, size, a, nd}
    end
  end

  def get_array({@ciruclar_buf, _, _, arr, _}) do
    arr
  end

  def get_value({@ciruclar_buf, _, _, _, d}, key) do
    d[key]
  end

  def get_dict({@ciruclar_buf, _, _, _, dict}) do
    dict
  end
end
