defmodule Utilities.Conversion do
  @moduledoc false
  require Utilities
  require Record

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

  def record_to_map(record) when Record.is_record(record) do
    record_name = record |> elem(0)

    mod_name =
      case record_name do
        :diameter_packet ->
          OnlineChargingSystem.Records.Diameter
      end

    Kernel.apply(mod_name, :record_to_map, [record])
  end

  # def map_to_record(record_kind, map) do
  #   Utilities.Conversion.Protocols.RecordMapConversion.map_to_record(record_kind, map)
  # end

  @compile {:inline, ip_address_tuple_to_string: 1}
  def ip_address_tuple_to_string(ip_address_tuple) when is_tuple(ip_address_tuple) do
    Utilities.erl_list_to_iex_string(:inet_parse.ntoa(ip_address_tuple))
  end

  @spec integer_to_inet_binary(integer(), integer()) :: binary()
  @compile {:inline, integer_to_inet_binary: 2}
  def integer_to_inet_binary(number, bin_length)
      when is_integer(number) and is_integer(bin_length) do
    bit_size = 8 * bin_length
    <<number::size(bit_size)>>
  end

  def nested_tuple_to_list(list) when is_list(list) do
    list
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(map) when is_map(map) do
    Enum.reduce(
      Map.to_list(map),
      %{},
      fn {k, v}, acc ->
        new_v =
          if is_list(v) or is_map(v) or is_tuple(v) do
            nested_tuple_to_list(v)
          else
            v
          end

        Map.put(acc, k, new_v)
      end
    )
  end

  def nested_tuple_to_list(x), do: x
end
