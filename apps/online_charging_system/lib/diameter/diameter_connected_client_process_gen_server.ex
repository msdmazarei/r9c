defmodule OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess do
  use GenServer

  require Logger
  require Utilities.Conversion
  require Utilities.Logging
  alias Utilities.Logging

  @client "client"
  @client_address "client_address"
  @client_ip "client_ip"
  @client_port "client_port"
  @client_config "client_config"
  @input_buffer "input_buffer"
  @input_packets "input_packets"
  @output_buffer "output_buffer"
  @received_bytes "received_bytes"
  @sent_bytes "sent_bytes"
  @trefs "trefs"
  @stats_in_packets "stats_in_packets"
  @stats_out_packets "stats_out_packets"
  @process_name "process_name"

  @last_arrived_message_timestamp "last_arrived_message_timestamp"

  @impl true
  def init(%{"client" => client_socket, "config" => client_configs}) do
    Logging.debug("Calld.")
    {:ok, {client_ip_tuple, client_port}} = :inet.peername(client_socket)
    client_ip_address_string = Utilities.Conversion.ip_address_tuple_to_string(client_ip_tuple)
    {:ok, tref_read} = :timer.send_interval(10, :read_socket)
    {:ok, tref_send} = :timer.send_interval(10, :send_to_socket)
    {:ok, tref_process} = :timer.send_interval(10, :process_received_packets)
    Logging.debug("return started successfully")
    now = Utilities.now()
    pname = "#{now};diameter_client::#{client_ip_address_string}:#{client_port}"
    pname = String.to_atom(pname)
    Process.register( self(),pname)

    {:ok,
     %{
       @process_name => pname,
       @client => client_socket,
       @client_address => "#{client_ip_address_string}:#{client_port}",
       @client_config => client_configs,
       @input_buffer => <<>>,
       @output_buffer => <<>>,
       @input_packets => [],
       @last_arrived_message_timestamp => Utilities.now(),
       @received_bytes => 0,
       @sent_bytes => 0,
       @client_ip => client_ip_address_string,
       @client_port => client_port,
       @trefs => [tref_read, tref_send, tref_process],
       @stats_in_packets => 0,
       @stats_out_packets => 0
     }}
  end

  @impl true
  def handle_call(
        {:send_diameter_packet, diameter_packet = %Utilities.Parsers.Diameter.DiameterPacket{}},
        _from,
        state = %{
          @output_buffer => output_buffer,
          @stats_out_packets => stats_out_packets
        }
      ) do
    Logging.debug("a diameter to send received ")
    # Logging.info("+++++++ DIAMETER SEND REQUEST +++++++ ")

    dia_bin = diameter_packet |> Utilities.Parsers.Diameter.serialize_to_bin()
    Logging.debug("successfully converted to binary")
    new_output_buffer = <<output_buffer::binary, dia_bin::binary>>
    # Logging.info("DIAMETER TO SEND ADDED ")

    new_state =
      state
      |> Map.put(@output_buffer, new_output_buffer)
      |> Map.put(@stats_out_packets, stats_out_packets + 1)

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_info(
        :process_received_packets,
        state = %{
          @input_packets => input_packets,
          @client => client,
          @client_config => client_config,
          @client_address => client_address,
          @client_ip => client_ip,
          @client_port => client_port,
          @process_name => pname
        }
      ) do
    input_packets
    |> Enum.map(fn x ->
      qn = get_queue_name_for_dia_packet(client, client_config, x, client_address)

      if qn != nil do
        Logging.debug("enqueue packet to q:~p", [qn])

        diameter_struct = %DatabaseEngine.Models.DiameterPacket{
          id: UUID.uuid1(),
          client_address: client_ip,
          client_port: client_port,
          capture_timestamp: Utilities.now(),
          packet_bin: x,
          parsed_packet: Utilities.Parsers.Diameter.parse_from_bin(x),
          options: %{},
          internal_callback: %DatabaseEngine.Models.InternalCallback{
            module_name: __MODULE__,
            function_name: :send_response_back_to_client,
            arguments: [{node(), pname}]
          }
        }

        :ok =
          DatabaseEngine.DurableQueue.enqueue(
            qn,
            diameter_struct
          )
      end
    end)

    new_state = state |> Map.put(@input_packets, [])
    {:noreply, new_state}
  end

  @impl true
  def handle_info(
        {:send_diameter_packet, diameter_packet = %Utilities.Parsers.Diameter.DiameterPacket{}},
        state = %{
          @output_buffer => output_buffer,
          @stats_out_packets => stats_out_packets
        }
      ) do
    Logging.debug("a diameter to send received ")
    # Logging.info("+++++++ DIAMETER SEND REQUEST +++++++ ")

    dia_bin = diameter_packet |> Utilities.Parsers.Diameter.serialize_to_bin()
    Logging.debug("successfully converted to binary")
    new_output_buffer = <<output_buffer::binary, dia_bin::binary>>
    # Logging.info("DIAMETER TO SEND ADDED ")

    new_state =
      state
      |> Map.put(@output_buffer, new_output_buffer)
      |> Map.put(@stats_out_packets, stats_out_packets + 1)

    {:noreply, new_state}
  end

  @impl true
  def handle_info(
        :read_socket,
        state = %{
          @input_buffer => input_buffer,
          @client => client,
          @input_packets => input_packets,
          @received_bytes => received_bytes,
          @stats_in_packets => stats_in_packets
        }
      ) do
    case read_from_tcp_socket(client, input_buffer, received_bytes) do
      {:terminate, reason} ->
        Logging.debug("stopping server cause of:~p", [reason])
        {:stop, {:reason, reason}, state}

      {rest_buf, dia_packets, received_bytes} ->
        new_state =
          state
          |> Map.put(@input_buffer, rest_buf)
          |> Map.put(@input_packets, input_packets ++ dia_packets)
          |> Map.put(@received_bytes, received_bytes)
          |> Map.put(@stats_in_packets, stats_in_packets + length(dia_packets))

        new_state =
          if length(dia_packets) > 0 do
            new_state
            |> Map.put(@last_arrived_message_timestamp, Utilities.now())
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
          @client => client,
          @client_address => client_address,
          @output_buffer => output_buffer,
          @sent_bytes => sent_bytes
        }
      ) do
    new_state =
      if byte_size(output_buffer) > 0 do
        Logging.debug("flushing buffer to socket ...")

        case :gen_tcp.send(client, output_buffer) do
          :ok ->
            sent_bytes = sent_bytes + byte_size(output_buffer)

            Logging.debug("buffer with len:~p sent successfully to client:~p", [
              byte_size(output_buffer),
              client_address
            ])

            state
            |> Map.put(@output_buffer, <<>>)
            |> Map.put(@sent_bytes, sent_bytes)

          {:error, reason} ->
            Logging.debug("Problem to send buffer to client:~p cause of ~p", [
              client_address,
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
  def handle_info(msg, state) do
    Logging.info("---------- MSG NOT HANDLEED ARRIVED:~p", [msg])
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state = %{@process_name => pname}) do
    Process.unregister(pname)
    Logging.debug("Called. reason:~p", [reason])
    release_resources(state)
    state
  end

  def release_resources(_state = %{@client => client, @trefs => trefs}) do
    trefs
    |> Enum.map(fn x ->
      :timer.cancel(x)
    end)

    :gen_tcp.close(client)
  end

  def send_response_back_to_client({n,pn}, script_state) do
    Logging.debug("Called. cun: ~p r-pid:~p scr:~p", [node(),{n,pn}, script_state.last_cel_result])


    case script_state.last_cel_result do
      {:return, [dia_packet = %Utilities.Parsers.Diameter.DiameterPacket{}]} ->
        # Logging.info("send dia_packet to process:~p", [pid])
        # GenServer.call(pid,{:send_diameter_packet, dia_packet})
        # r=:rpc.call(pid|>node, GenServer, :call, [pid,{:send_diameter_packet, dia_packet}],5000)
        # Logging.info("result rpc call:~p")
        :erlang.send({pn,n}, {:send_diameter_packet, dia_packet})
        # send(pid, {:send_diameter_packet, dia_packet})

      v ->
        Logging.warn("unexpected result from script: ~p", [v])
    end
  end

  def detect_diameter_packet(buf, diameters \\ []) do
    Logging.debug("called. with buf-len:~p, dia-len:~p", [
      byte_size(buf || <<>>),
      length(diameters)
    ])

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
            Logging.debug("no diameter packet found.")
            {diameters, buf}
        end

      _ ->
        Logging.debug("no diameter packet found")
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
        Logging.debug("new packet arrived. it is packet-length:~p", [byte_size(packet)])
        received_bytes = received_bytes + byte_size(packet)
        new_buf = buf <> packet
        {dia_packets, rest_buf} = detect_diameter_packet(new_buf)
        Logging.debug("dia_packet - length: ~p", [dia_packets |> length])

        {rest_buf, dia_packets, received_bytes}
    end
  end

  def get_queue_name_for_dia_packet(_client, client_conf, diameter_bin_packet, client_address) do
    Logging.debug("Called.")

    case diameter_bin_packet do
      <<_::binary-size(8), application_id::size(32), _::binary>> ->
        application_config = client_conf[application_id] || client_conf["default"]

        if application_config != nil do
          if application_config["queue"] != nil do
            application_config["queue"]
          else
            Logging.warn(
              "no queue definition found in existing config(bad config) for client:~p and application_id:~p",
              [
                client_address,
                application_id
              ]
            )

            nil
          end
        else
          Logging.warn("no application config found for client :~p and application_id:~p", [
            client_address,
            application_id
          ])

          nil
        end

      _ ->
        nil
    end
  end
end
