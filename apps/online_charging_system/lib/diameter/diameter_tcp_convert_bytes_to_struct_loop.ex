defmodule OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess.ConvertBytesToStructLoop do
  @mnesia_buffer_key "mnesia_buffer_key"
  @mnesia_packets_key "mnesia_packets_key"
  @master_pid "master_pid"
  @terminate_reason "terminate_reason"
  @processed_bytes "processed_bytes"
  @generated_packets "generated_packets"
  @bytes_to_struct_time_ms "bytes_to_struct_time_ms"
  @bytes_to_struct_time_ms_detection_part "bytes_to_struct_time_ms_detection_part"
  @bytes_to_struct_time_ms_mnesia_part "bytes_to_struct_time_ms_mnesia_part"

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.LKV
  alias DatabaseEngine.Interface.LKV

  def detect_diameter_packet(buf, diameters \\ []) do
    case buf do
      <<_, packet_length::size(24), _::binary>> ->
        Logging.debug("searching for dia packet with length : ~p", [packet_length])

        case buf do
          <<diameter_bin_packet::binary-size(packet_length), rest::binary>> ->
            Logging.debug(
              "diameter packet found.dia_packet_leng:~p, rest:~p",
              [byte_size(diameter_bin_packet), byte_size(rest)]
            )

            all_dias = [diameter_bin_packet | diameters]
            detect_diameter_packet(rest, all_dias)

          _ ->
            # Logging.debug("no diameter packet found.")
            {diameters, buf}
        end

      _ ->
        # Logging.debug("no diameter packet found")
        {diameters, buf}
    end
  end

  def convertBytesToStruct(mnesia_buffer_key, mnesia_packets_key) do
    in_buf = LKV.get(mnesia_buffer_key) || <<>>

    st_detection = Utilities.now()
    {dia_packets, rest_buf} = detect_diameter_packet(in_buf)
    du_detection = Utilities.now() - st_detection

    packets_in_count = dia_packets |> length

    if packets_in_count > 0 do
      Logging.debug("dia_packets are arrived. count:~p", [packets_in_count])

      st_mnesia = Utilities.now()

      r =
        :mnesia.transaction(fn ->
          # finding diameter packets is a cpu cosuming so we separated
          # processing and storing phases
          # we avoid to lock buffer when we are searching dia_packets
          # meanwhile some new bytes may arrived
          current_in_buf = LKV.get_for_update(mnesia_buffer_key) || []
          already_packets = LKV.get_for_update(mnesia_packets_key) || []

          in_buf_len = byte_size(in_buf)
          <<_::binary-size(in_buf_len), new_arrived_bytes::binary>> = current_in_buf

          new_in_buf = <<rest_buf::binary, new_arrived_bytes::binary>>
          new_packets = already_packets ++ dia_packets

          LKV.set(mnesia_buffer_key, new_in_buf)
          LKV.set(mnesia_packets_key, new_packets)
        end)

      du_mnesia = Utilities.now() - st_mnesia

      case r do
        {:atomic, _} ->
          bytes_processes = byte_size(in_buf) - byte_size(rest_buf)
          generated_packets = length(dia_packets)
          {{bytes_processes, generated_packets}, du_detection, du_mnesia}

        {:aborted, reason} ->
          Logging.error("problem in mnesia: ~p", [reason])
          {:terminate, reason}
      end
    else
      {{0, 0}, 0, 0}
    end
  end

  def loop(
        state = %{
          @mnesia_buffer_key => mnesia_buffer_key,
          @mnesia_packets_key => mnesia_packets_key,
          @processed_bytes => processed_bytes,
          @generated_packets => generated_packets
        }
      ) do
    bytes_to_struct_time_ms = state[@bytes_to_struct_time_ms] || 0
    bytes_to_struct_time_ms_detection_part = state[@bytes_to_struct_time_ms_detection_part] || 0

    bytes_to_struct_time_ms_mnesia_part = state[@bytes_to_struct_time_ms_mnesia_part] || 0

    {continue, state} =
      receive do
        income_msg ->
          case income_msg do
            {sender, back_reference, :stats} ->
              send(
                sender,
                {back_reference,
                 %{
                   @processed_bytes => processed_bytes,
                   @generated_packets => generated_packets,
                   @bytes_to_struct_time_ms => bytes_to_struct_time_ms,
                   @bytes_to_struct_time_ms_mnesia_part => bytes_to_struct_time_ms_mnesia_part,
                   @bytes_to_struct_time_ms_detection_part =>
                     bytes_to_struct_time_ms_detection_part
                 }}
              )

              {true, state}

            _ ->
              {true, state}
          end
      after
        0 ->
          st_conversion = Utilities.now()
          r = convertBytesToStruct(mnesia_buffer_key, mnesia_packets_key)
          du_conversion = Utilities.now() - st_conversion

          case r do
            {:terminate, reason} ->
              state = state |> Map.put(@terminate_reason, reason)
              {false, state}

            {{0, _}, _, _} ->
              {true, state}

            {{pbys, gpkts}, du_bytes_to_struct_time_ms_detection_part,
             du_bytes_to_struct_time_ms_mnesia_part}
            when is_number(pbys) and pbys > 0 ->
              new_state =
                state
                |> Map.put(@processed_bytes, (processed_bytes || 0) + pbys)
                |> Map.put(@generated_packets, (generated_packets || 0) + gpkts)
                |> Map.put(@bytes_to_struct_time_ms, bytes_to_struct_time_ms + du_conversion)
                |> Map.put(
                  @bytes_to_struct_time_ms_detection_part,
                  bytes_to_struct_time_ms_detection_part +
                    du_bytes_to_struct_time_ms_detection_part
                )
                |> Map.put(
                  @bytes_to_struct_time_ms_mnesia_part,
                  bytes_to_struct_time_ms_mnesia_part + du_bytes_to_struct_time_ms_mnesia_part
                )

              {true, new_state}
          end
      end

    if continue do
      loop(state)
    else
      Process.exit(self(), state[@terminate_reason])
    end
  end
end
