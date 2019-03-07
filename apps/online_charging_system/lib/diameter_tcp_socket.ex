defmodule OnlineChargingSystem.Servers.Diameter.TcpSocket do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities.Conversion

  require DatabaseEngine.Models.DiameterPacket
  require DatabaseEngine.Models.InternalCallback

  @tcp_recv_timeout 30_000

  @compile {:inline, get_peer_ip_address_as_string: 1}

  def start_listen(port_no \\ 3868, ip \\ {127, 0, 0, 1}) do
    Logging.debug("Called. ip: ~p port: ~p", [ip, port_no])

    {:ok, lSock} =
      :gen_tcp.listen(port_no,
        ip: ip,
        port: port_no,
        active: false,
        mode: :binary,
        packet: :raw
      )

    Logging.debug("server started successfully")

    Process.spawn(
      fn ->
        wait_to_connection(lSock)
      end,
      []
    )
  end

  def wait_to_connection(lsocket) do
    # should read from config file or
    # mnesia or anywhere else
    # config has hirarchy structer as below:
    # { server_ip => { application_id => { user_process_name => %{"queue": "1" }}}}
    diameter_server_config = %{
      "192.168.1.1" => %{},
      "default" => %{
        "default" => %{
          "queue" => "diameter_queue"
        }
      }
    }

    Logging.debug(" waiting for new connection from clients")
    {:ok, client} = :gen_tcp.accept(lsocket)
    Logging.debug("new connection arrived from: ~p", [:inet.peername(client)])
    {:ok, {client_ip_address_tuple, _client_port}} = :inet.peername(client)
    client_ip_address = Utilities.Conversion.ip_address_tuple_to_string(client_ip_address_tuple)
    client_config = diameter_server_config[client_ip_address] || diameter_server_config["default"]
    Kernel.spawn(__MODULE__, :server_diameter_client, [client, client_config])
    wait_to_connection(lsocket)
  end

  defp get_peer_ip_address_as_string(client_sock) do
    {:ok, {client_ip_address_tuple, _client_port}} = :inet.peername(client_sock)
    Utilities.Conversion.ip_address_tuple_to_string(client_ip_address_tuple)
  end

  def send_response_back_to_client(pid, packet_bin_to_send) do
    Logger.debug("Called. with params pid:~p packet_bin_to_send:~p",[
      pid,packet_bin_to_send
    ])
    # send pid, packet_bin_to_send
  end

  def server_diameter_client(client, client_config, buf \\ <<>>) do
    {:ok, {client_ip_tuple, client_port}} = :inet.peername(client)
    client_ip_address_string = Utilities.Conversion.ip_address_tuple_to_string(client_ip_tuple)

    Logging.debug("waiting to  new data from client. old-buf-len:~p", [byte_size(buf)])

    case :gen_tcp.recv(client, 0, @tcp_recv_timeout) do
      {:error, reason} ->
        Logging.error("problem happen to socket read. error:~p", [reason])

      {:ok, packet} ->
        Logging.debug("new packet arrived. it is packet-length:~p", [byte_size(packet)])
        new_buf = buf <> packet
        {dia_packets, rest_buf} = detect_diameter_packet(new_buf)
        Logging.debug("dia_packet - length: ~p", [dia_packets |> length])

        dia_packets
        |> Enum.map(fn x ->
          qn = get_queue_name_for_dia_packet(client, client_config, x)

          if qn != nil do
            Logging.debug("enqueue packet to q:~p", [qn])

            diameter_struct = %DatabaseEngine.Models.DiameterPacket{

              id: UUID.uuid1(),
              client_address: client_ip_address_string,
              client_port: client_port,
              capture_timestamp: Utilities.now(),
              packet_bin: x,
              parsed_packet: Utilities.Parsers.Diameter.parse_from_bin(x),
              options: %{},
              internal_callback: nil

              # %DatabaseEngine.Models.InternalCallback {
              #   module_name: __MODULE__,
              #   function_name: "send_response_back_to_client",
              #   arguments: [self()]
              # }
            }

            :ok =
              DatabaseEngine.DurableQueue.enqueue(
                qn,
                diameter_struct
              )
          end
        end)

        server_diameter_client(client, client_config, rest_buf)
    end

    Logging.debug("exit from process: ~p client:~p", [self(), client])
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

  def get_queue_name_for_dia_packet(client, client_conf, diameter_bin_packet) do
    Logging.debug("Called.")
    <<_::binary-size(8), application_id::size(32), _::binary>> = diameter_bin_packet
    application_config = client_conf[application_id] || client_conf["default"]

    if application_config != nil do
      if application_config["queue"] != nil do
        application_config["queue"]
      else
        Logging.warn(
          "no queue definition found in existing config(bad config) for client:~p and application_id:~p",
          [
            get_peer_ip_address_as_string(client),
            application_id
          ]
        )

        nil
      end
    else
      Logging.warn("no application config found for client :~p and application_id:~p", [
        get_peer_ip_address_as_string(client),
        application_id
      ])

      nil
    end
  end

end
