defmodule Utilities.Parsers.Diameter.DiameterPacket do
  defstruct version: 1,
            message_length: 0,
            request_bit: 0,
            proxiable_bit: 0,
            error_bit: 0,
            potentially_retransmit: 0,
            command_code: 0,
            application_id: 0,
            hop_by_hop_id: 0,
            end_to_end_id: 0,
            avps: []
end

defmodule Utilities.Parsers.Diameter.AVP do
  defstruct avp_code: 0,
            vendor_specific: 0,
            mandatory: 0,
            protected: 0,
            avp_length: 0,
            bin_value: <<>>,
            # below are logical and generally use to construct binary from value
            avp_type: nil,
            avp_value: nil
end

defmodule Utilities.Parsers.Diameter do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def parse_from_bin(bin) when is_binary(bin) and byte_size(bin) > 4 do
    rtn = %{}

    <<version::size(8), message_len::size(24), rest_1::binary>> = bin

    # rtn =
    #   rtn
    #   |> Map.put("version", version)
    #   |> Map.put("message_length", message_len)

    if message_len == byte_size(bin) do
      <<request_bit::size(1), proxiable_bit::size(1), error_bit::size(1),
        potentially_retransmit::size(1), _::size(4), command_code::size(24),
        rest_2::binary>> = rest_1

      <<application_id::size(32), hop_by_hop_id::size(32), end_to_end_id::size(32),
        rest_3::binary>> = rest_2

      # rtn =
      #   rtn
      #   |> Map.put("request_bit", request_bit)
      #   |> Map.put("proxiable_bit", proxiable_bit)
      #   |> Map.put("error_bit", error_bit)
      #   |> Map.put("potentially_retransmit", potentially_retransmit)
      #   |> Map.put("command_code", command_code)

      # rtn =
      #   rtn
      #   |> Map.put("application_id", application_id)
      #   |> Map.put("hop_by_hop_id", hop_by_hop_id)
      #   |> Map.put("end_to_end_id", end_to_end_id)

      # rtn = rtn |> Map.put("avps", parse_avps(rest_3, []))
      # rtn

      %Utilities.Parsers.Diameter.DiameterPacket{
        version: version,
        message_length: message_len,
        request_bit: request_bit,
        proxiable_bit: proxiable_bit,
        error_bit: error_bit,
        potentially_retransmit: potentially_retransmit,
        command_code: command_code,
        application_id: application_id,
        hop_by_hop_id: hop_by_hop_id,
        end_to_end_id: end_to_end_id,
        avps: parse_avps(rest_3, [])
      }
    else
      Logging.debug("message_len and binary size are different", [message_len, byte_size(bin)])
      nil
    end
  end

  def parse_from_bin(_) do
    nil
  end

  def parse_avps(avps_bin, list_of_parsed_avps)
      when is_binary(avps_bin) and byte_size(avps_bin) > 8 do
    <<avp_code::size(32), vendor_specific::size(1), mandatory::size(1), protected::size(1),
      _::size(5), avp_length::size(24), rest_1::binary>> = avps_bin

    # Logging.debug("avp_code: ~p [len= ~p]",[avp_code, avp_length])
    if byte_size(avps_bin) >= avp_length do
      # new_avp = %{
      #   "avp_code" => avp_code,
      #   "vendor_specific" => vendor_specific,
      #   "mandatory" => mandatory,
      #   "protected" => protected,
      #   "avp_length" => avp_length
      # }

      value_length = Kernel.ceil(avp_length / 4) * 4 - 8

      <<avp_bin_value::binary-size(value_length), rest_2::binary>> = rest_1
      # Logging.debug("avp_code: ~p len: ~p value: ~p ",[avp_code, value_length, avp_bin_value])
      # new_avp = new_avp |> Map.put("bin_value", avp_bin_value)
      new_avp = %Utilities.Parsers.Diameter.AVP{
        avp_code: avp_code,
        vendor_specific: vendor_specific,
        mandatory: mandatory,
        protected: protected,
        avp_length: avp_length,
        bin_value: avp_bin_value
      }

      parse_avps(rest_2, [new_avp | list_of_parsed_avps])
    else
      Logging.debug("byte_size(avp_bin) is lesser than avp leng: ~p  < ~p", [
        byte_size(avps_bin),
        avp_length
      ])

      list_of_parsed_avps
    end
  end

  def parse_avps(_, list_of_parsed_avps) do
    list_of_parsed_avps
  end

  def serialize_to_bin(%Utilities.Parsers.Diameter.DiameterPacket{
        version: version,
        request_bit: request_bit,
        proxiable_bit: proxiable_bit,
        error_bit: error_bit,
        potentially_retransmit: potentially_retransmit,
        command_code: command_code,
        application_id: application_id,
        hop_by_hop_id: hop_by_hop_id,
        end_to_end_id: end_to_end_id,
        avps: avps
      }) do
    avp_bins = avps |> Enum.map(&serialize_to_bin/1)

    message_len =
      div(
        8 + 24 + 1 + 1 + 1 + 1 + 4 + 24 + 32 + 32 + 32,
        8
      ) +
        (avp_bins
         |> Enum.map(&byte_size/1)
         |> Enum.sum())

    avps_bin =
      avp_bins
      |> Enum.reduce(<<>>, fn b, acc ->
        <<acc::binary, b::binary>>
      end)

    <<version::size(8), message_len::size(24), request_bit::size(1), proxiable_bit::size(1),
      error_bit::size(1), potentially_retransmit::size(1), 0::size(4), command_code::size(24),
      application_id::size(32), hop_by_hop_id::size(32), end_to_end_id::size(32),
      avps_bin::binary>>
  end

  def serialize_to_bin(%Utilities.Parsers.Diameter.AVP{
        avp_code: avp_code,
        vendor_specific: vendor_specific,
        mandatory: mandatory,
        protected: protected,
        avp_type: avp_type,
        avp_value: avp_value
      }) do
    rtn =
      <<avp_code::size(32), vendor_specific::size(1), mandatory::size(1), protected::size(1),
        0::size(5)>>

    rtn =
      case avp_type do
        "OctetString" ->
          rtn

        "Integer32" ->
          rtn

        "Integer64" ->
          rtn

        "Unsigned32" ->
          avp_length = byte_size(rtn) + div(32, 8)
          <<rtn::binary(), avp_length::size(24), avp_value::size(32)>>

        "Unsigned64" ->
          avp_length = byte_size(rtn) + div(64, 8)
          <<rtn::binary(), avp_length::size(24), avp_value::size(32)>>

        "Float32" ->
          rtn

        "Float64" ->
          rtn

        "Address" ->
          rtn

        "Time" ->
          rtn

        "UTF8String" ->
          rtn

        "DiameterIdentity" ->
          rtn

        "DiameterURI" ->
          rtn

        "Enumerated" ->
          rtn

        "IPFilterRule" ->
          rtn

        "QoSFilterRule" ->
          rtn
      end

    rtn
  end
end
