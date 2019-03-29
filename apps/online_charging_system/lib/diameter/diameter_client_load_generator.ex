defmodule OnlineChargingSystem.Diameter.LoadClient do
  use GenServer

  require Logger
  require Utilities.Conversion
  require Utilities.Logging
  alias Utilities.Logging

  @socket "socket"
  @ip_address "ip_address"
  @tcp_port "tcp_port"
  @timers "timers"
  @process_name "process_name"
  @user_batch_map "user_batch_map"
  @last_sent_epoch "last_sent_epoch"
  @app_start_time "app_start_time"
  @batch_send_time_gap "batch_send_time_gap"
  @username_prefix "username_prefix"
  @out_buf "out_buf"
  @expire_map "expire_map"
  @wait_for_ack "wait_for_ack"
  @ack_timeout "ack_timeout"
  @in_buffer "in_buffer"
  @received_bytes "received_bytes"
  @input_packets "input_packets"
  @expired_count "expired_count"
  @rcvd_pkt "rcvd_pkt"
  @sent_pkts "sent_pkts"
  @dia_application_id 3_285_213
  @dia_command_code 271

  def print_state(
        func_name,
        state = %{
          @last_sent_epoch => last_sent_epoch,
          @expire_map => expire_map,
          @wait_for_ack => wait_for_ack,
          @expired_count => expired_count,
          @rcvd_pkt => rcvd_pkt,
          @sent_pkts => sent_pkts
        }
      ) do
    Logging.info(
      "~n funcname: ~p ~n last_sent:~p ~n expire:~p~n ack:~p~n expired_count:~p in_pkts:~p sent_pkts:~p~n-------------~n",
      [
        func_name,
        last_sent_epoch,
        expire_map,
        wait_for_ack,
        expired_count,
        rcvd_pkt,
        sent_pkts
      ]
    )
  end

  @impl true
  def init(a) do
    Logging.debug("Called.")

    %{
      "ip" => ip_address,
      "port" => tcp_port,
      @username_prefix => username_prefix,
      "batch_count" => batch_members_count,
      @batch_send_time_gap => batch_send_time_gap,
      "start_no" => start_no,
      "user_count" => user_count,
      @process_name => process_name,
      @ack_timeout => ack_timeout
    } = a

    user_batch_map =
      :lists.seq(start_no, start_no + user_count - 1)
      |> Enum.map(fn i ->
        batch_no = div(i - start_no, batch_members_count)
        {batch_no, i}
      end)
      |> Enum.group_by(fn {k, _} -> k end, fn {_, v} -> v end)

    ip_address =
      case ip_address do
        i when is_binary(i) ->
          i

        {_, _, _, _} ->
          Utilities.erl_list_to_iex_string(:inet.ntoa(ip_address))

        {_, _, _, _, _, _, _, _} ->
          Utilities.erl_list_to_iex_string(:inet.ntoa(ip_address))
      end

    {:ok, server_ip_address} = :inet.parse_address(Utilities.to_erl_list(ip_address))

    {:ok, sock} =
      :gen_tcp.connect(
        server_ip_address,
        tcp_port,
        active: false,
        mode: :binary,
        packet: :raw
      )

    Logging.debug("connected  successfully")
    {:ok, tref_check_in_data} = :timer.send_interval(300, :read_socket)
    {:ok, tref_send_data} = :timer.send_interval(300, :send_to_socket)
    {:ok, tref_send_req} = :timer.send_interval(3000, :send_req)
    {:ok, tref_check_expire} = :timer.send_interval(3000, :check_expire)
    {:ok, tref_process_in_packets} = :timer.send_interval(1000, :process_in_packets)

    {:ok,
     %{
       "ip" => ip_address,
       "port" => tcp_port,
       @socket => sock,
       @timers => [tref_check_in_data, tref_send_data, tref_check_expire, tref_send_req],
       @wait_for_ack => %{},
       @expire_map => %{},
       @last_sent_epoch => %{},
       @user_batch_map => user_batch_map,
       @in_buffer => <<>>,
       @out_buf => <<>>,
       @process_name => process_name,
       @app_start_time => Utilities.now(),
       @batch_send_time_gap => batch_send_time_gap,
       @ack_timeout => ack_timeout,
       @received_bytes => 0,
       @input_packets => [],
       @username_prefix => username_prefix,
       @expired_count => 0,
       @rcvd_pkt => 0,
       @sent_pkts => 0
     }}
  end

  def get_diameter_packet_bin_to_send_to(
        batch_no,
        user_no,
        state = %{
          @username_prefix => username_prefix
        }
      ) do
    ack_str = Utilities.randint(100_000_000) |> to_string()
    username = "#{username_prefix}#{user_no}"

    dia_pkt = %Utilities.Parsers.Diameter.DiameterPacket{
      application_id: @dia_application_id,
      command_code: @dia_command_code,
      end_to_end_id: 0,
      error_bit: 0,
      hop_by_hop_id: 0,
      request_bit: 1,
      proxiable_bit: 0,
      version: 1,
      avps: [
        %Utilities.Parsers.Diameter.AVP{
          avp_code: 1,
          avp_type: "OctetString",
          avp_value: username
        },
        %Utilities.Parsers.Diameter.AVP{
          avp_code: 263,
          avp_type: "OctetString",
          avp_value: ack_str
        }
      ]
    }

    {ack_str, dia_pkt |> Utilities.Parsers.Diameter.serialize_to_bin()}
  end

  def detect_diameter_packet(buf, diameters \\ []) do
    # Logging.debug("called. with buf-len:~p, dia-len:~p", [
    #   byte_size(buf || <<>>),
    #   length(diameters)
    # ])

    case buf do
      <<_, packet_length::size(24), _::binary>> ->
        # Logging.debug("searching for dia packet with length : ~p", [packet_length])

        case buf do
          <<diameter_bin_packet::binary-size(packet_length), rest::binary>> ->
            # Logging.debug(
            #   "diameter packet found.dia_packet_leng:~p, rest:~p",
            #   [byte_size(diameter_bin_packet), byte_size(rest)]
            # )

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

  def read_from_tcp_socket(client, buf, received_bytes) do
    case :gen_tcp.recv(client, 0, 3) do
      {:error, :timeout} ->
        {buf, [], received_bytes}

      {:error, reason} ->
        Logging.debug("problem in client socket reading. reason:~p. kill me", [reason])
        {:terminate, reason}

      # Process.exit(self()ormal)

      {:ok, packet} ->
        # Logging.debug("new packet arrived. it is packet-length:~p", [byte_size(packet)])
        received_bytes = received_bytes + byte_size(packet)
        new_buf = buf <> packet
        {dia_packets, rest_buf} = detect_diameter_packet(new_buf)
        Logging.debug("dia_packet - length: ~p", [dia_packets |> length])

        {rest_buf, dia_packets, received_bytes}
    end
  end

  @impl true
  def handle_info(
        :check_expire,
        state = %{
          @expire_map => expire_map,
          @wait_for_ack => wait_for_ack,
          @expired_count => expired_count
        }
      ) do
    Logging.debug("check_expires called.")
    now = Utilities.now()
    expired_keys = Map.keys(expire_map) |> Enum.filter(fn x -> now > x end)

    valid_expire_keys =
      expired_keys
      |> Enum.filter(fn x ->
        un = expire_map[x]
        wait_for_ack[un] != nil
      end)

    if length(valid_expire_keys) > 0 do
      Logging.debug("valid_expired_keys:~p", [valid_expire_keys])
    end

    expired_count = expired_count + length(valid_expire_keys)

    wait_for_ack =
      valid_expire_keys
      |> Enum.reduce(wait_for_ack, fn k, w4ack ->
        v = expire_map[k]
        w4ack |> Map.delete(v)
      end)

    expire_map =
      expired_keys
      |> Enum.reduce(expire_map, fn k, exmap ->
        exmap |> Map.delete(k)
      end)

    new_state =
      state
      |> Map.put(@expire_map, expire_map)
      |> Map.put(@wait_for_ack, wait_for_ack)
      |> Map.put(@expired_count, expired_count)

    print_state("check_expire_returns", new_state)
    {:noreply, new_state}
  end

  @impl true
  def handle_info(
        :process_in_packets,
        state = %{
          @input_packets => input_packets,
          @wait_for_ack => wait_for_ack,
          @username_prefix => username_prefix,
          @rcvd_pkt => rcvd_pkt
        }
      ) do
    if length(input_packets) > 0 do
      rcvd_pkt = rcvd_pkt + length(input_packets)
      Logging.debug("processing input packets")

      username_sessid_tuples =
        input_packets
        |> Enum.map(fn bin_pkt ->
          parsed_pkt = Utilities.Parsers.Diameter.parse_from_bin(bin_pkt)

          username_avp =
            parsed_pkt.avps
            |> Enum.filter(fn avp ->
              avp.avp_code == 1
            end)

          session_id_avp =
            parsed_pkt.avps
            |> Enum.filter(fn x ->
              x.avp_code == 263
            end)

          if session_id_avp == [] or username_avp == [] do
            nil
          else
            username_avp = username_avp |> hd
            session_id_avp = session_id_avp |> hd

            {
              Utilities.Parsers.Diameter.avp_octet_string_value(username_avp),
              Utilities.Parsers.Diameter.avp_octet_string_value(session_id_avp)
            }
          end
        end)

      username_sessid_tuples = username_sessid_tuples |> Enum.filter(fn x -> x != nil end)
      Logging.debug("user_sessid list:~p", [username_sessid_tuples])

      prefix_len = byte_size(username_prefix)

      wait_for_ack =
        username_sessid_tuples
        |> Enum.reduce(wait_for_ack, fn {u, ssid}, w4ack ->
          {un, _} = String.slice(u, prefix_len..-1) |> Integer.parse()
          Logging.debug("checking un:~p ssid:~p", [un, ssid])

          if w4ack[un] == ssid do
            Logging.debug("un:~p ssid:~p is ok", [un, ssid])
            w4ack |> Map.delete(un)
          else
            w4ack
          end
        end)

      new_state =
        state
        |> Map.put(@wait_for_ack, wait_for_ack)
        |> Map.put(@input_packets, [])
        |> Map.put(@rcvd_pkt, rcvd_pkt)

      print_state("process_in_packets", new_state)

      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(
        :read_socket,
        state = %{
          @in_buffer => input_buffer,
          @socket => client,
          @received_bytes => received_bytes,
          @input_packets => input_packets
        }
      ) do
    case read_from_tcp_socket(client, input_buffer, received_bytes) do
      {:terminate, reason} ->
        Logging.debug("stopping server cause of:~p", [reason])
        {:stop, {:reason, reason}, state}

      {rest_buf, dia_packets, received_bytes} ->
        new_state =
          state
          |> Map.put(@in_buffer, rest_buf)
          |> Map.put(@input_packets, input_packets ++ dia_packets)
          |> Map.put(@received_bytes, received_bytes)

        new_state =
          if length(dia_packets) > 0 do
            new_state
          else
            new_state
          end

        {:noreply, new_state}
    end
  end

  @impl true
  def handle_info(
        :send_to_socket,
        state = %{
          @socket => client,
          @out_buf => output_buffer,
          @process_name => process_name
        }
      ) do
    new_state =
      if byte_size(output_buffer) > 0 do
        Logging.debug("flushing buffer to socket ...")

        case :gen_tcp.send(client, output_buffer) do
          :ok ->
            Logging.debug("buffer with len:~p sent successfully to client:~p", [
              byte_size(output_buffer),
              process_name
            ])

            state
            |> Map.put(@out_buf, <<>>)

          {:error, reason} ->
            Logging.debug("Problem to send buffer to client:~p cause of ~p", [
              process_name,
              reason
            ])

            state
        end
      else
        state
      end

    {:noreply, new_state}
  end

  @impl true
  def handle_info(
        :send_req,
        state = %{
          @user_batch_map => user_batch_map,
          @last_sent_epoch => last_sent_epoch,
          @app_start_time => app_start_time,
          @batch_send_time_gap => batch_send_time_gap,
          @wait_for_ack => wait_for_ack,
          @expire_map => expire_map,
          @out_buf => out_buf,
          @ack_timeout => ack_timeout,
          @sent_pkts => sent_pkts
        }
      ) do
    batch_nos_to_send_to =
      Map.keys(user_batch_map)
      |> Enum.filter(fn bn ->
        if last_sent_epoch[bn] == nil do
          if Utilities.now() >= bn * 7_000 + app_start_time do
            true
          else
            false
          end
        else
          if Utilities.now() >= last_sent_epoch[bn] + batch_send_time_gap do
            true
          else
            false
          end
        end
      end)

    now = Utilities.now()

    last_sent_epoch =
      batch_nos_to_send_to
      |> Enum.reduce(last_sent_epoch, fn bn, acc ->
        acc |> Map.put(bn, now)
      end)

    batch_user_tuples =
      batch_nos_to_send_to
      |> Enum.map(fn x ->
        user_batch_map[x]
        |> Enum.map(fn y ->
          {x, y}
        end)
      end)
      |> List.flatten()

    s = {out_buf, expire_map, wait_for_ack}

    {out_buf, expire_map, wait_for_ack} =
      batch_user_tuples
      |> Enum.reduce(s, fn {bn, un}, {out_buf, expire_map, wait_for_ack} ->
        {ack, pkt_bin} = get_diameter_packet_bin_to_send_to(bn, un, state)
        out_buf = <<out_buf::binary, pkt_bin::binary>>
        expire_map = expire_map |> Map.put(Utilities.now() + ack_timeout, un)
        wait_for_ack = wait_for_ack |> Map.put(un, ack)
        {out_buf, expire_map, wait_for_ack}
      end)

    state =
      state
      |> Map.put(@expire_map, expire_map)
      |> Map.put(@out_buf, out_buf)
      |> Map.put(@wait_for_ack, wait_for_ack)
      |> Map.put(@last_sent_epoch, last_sent_epoch)
      |> Map.put(@sent_pkts, sent_pkts + length(batch_user_tuples))

    print_state("send_req", state)

    {:noreply, state}
  end

  @impl true
  def terminate(
        reason,
        state = %{
          @timers => timers,
          @process_name => process_name,
          @socket => socket
        }
      ) do
    Logging.info("~p client is going to exit", [
      process_name
    ])

    timers
    |> Enum.map(fn x ->
      :timer.cancel(x)
    end)

    :gen_tcp.close(socket)
    state
  end
end
