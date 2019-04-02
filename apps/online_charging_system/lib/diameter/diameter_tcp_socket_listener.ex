defmodule OnlineChargingSystem.Servers.Diameter.TcpServer do
  use GenServer

  require Logger
  require Utilities.Conversion
  require Utilities.Logging
  alias Utilities.Logging

  @listen_socket "listen_socket"
  @ip_address "ip_address"
  @tcp_port "tcp_port"
  @connected_client_pids "connected_client_pids"
  @disconnected_client_count "disconnected_client_count"
  @check_incoming_connection_tref "check_incoming_connection_tref"
  @diameter_server_config "diameter_server_config"
  @socket_process_map "socket_process_map"

  @impl true
  def init(%{"ip" => ip_address, "port" => tcp_port}) do
    Logging.debug("Called.")

    ip_address =
      case ip_address do
        i when is_binary(i) ->
          i

        {_, _, _, _} ->
          Utilities.erl_list_to_iex_string(:inet.ntoa(ip_address))

        {_, _, _, _, _, _, _, _} ->
          Utilities.erl_list_to_iex_string(:inet.ntoa(ip_address))
      end

    {:ok, listen_ip_address} = :inet.parse_address(Utilities.to_erl_list(ip_address))

    {:ok, lSock} =
      :gen_tcp.listen(tcp_port,
        ip: listen_ip_address,
        port: tcp_port,
        active: false,
        mode: :binary,
        packet: :raw
      )

    Logging.debug("server started successfully")

    case :timer.send_interval(100, :check_for_incoming_connection) do
      {:ok, tref} ->
        Process.flag(:trap_exit, true)

        {:ok,
         %{
           @listen_socket => lSock,
           @ip_address => ip_address,
           @tcp_port => tcp_port,
           @connected_client_pids => [],
           @disconnected_client_count => 0,
           @check_incoming_connection_tref => tref,
           @socket_process_map => %{},
           @diameter_server_config => %{
             "192.168.1.1" => %{},
             "default" => %{
               "default" => %{
                 "queue" => "diameter_queue"
               }
             }
           }
         }}

      {:error, reason} ->
        :gen_tcp.close(lSock)
        {:stop, reason}
    end
  end

  @impl true
  def handle_call(
        :packets_stat,
        _from,
        state = %{
          @connected_client_pids => connected_client_pids
        }
      ) do
    per_client =
      connected_client_pids
      |> Enum.map(fn xpid ->
        s = :sys.get_state(xpid)
        s["stats"]
      end)

    {in_, out_} =
      per_client
      |> Enum.reduce({0, 0}, fn m, {i, o} ->
        {i + (m["in"] || 0), o + (m["out"] || 0)}
      end)

    {
      :reply,
      %{
        "per_client" => per_client,
        "in" => in_,
        "out" => out_
      },
      state
    }
  end

  @impl true
  def handle_info(
        :check_for_incoming_connection,
        state = %{
          @listen_socket => listen_socket,
          @connected_client_pids => connected_client_pids,
          @diameter_server_config => diameter_server_config,
          @socket_process_map => socket_process_map
        }
      ) do
    case :gen_tcp.accept(listen_socket, 50) do
      {:error, :timeout} ->
        {:noreply, state}

      {:error, reason} ->
        Logging.error("could not accept connection cause of: ~p", [reason])
        {:stop, reason}

      {:ok, client} ->
        {:ok, {client_ip_address_tuple, client_port}} = :inet.peername(client)

        client_ip_address =
          Utilities.Conversion.ip_address_tuple_to_string(client_ip_address_tuple)

        Logging.info("a connection arrived from ~p:~p", [client_ip_address, client_port])

        client_config =
          diameter_server_config[client_ip_address] || diameter_server_config["default"]

        case GenServer.start_link(OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess, %{
               "client" => client,
               "config" => client_config
             }) do
          {:ok, client_pid} ->
            new_socket_process_map = socket_process_map |> Map.put(client, client_pid)

            new_state =
              state
              |> Map.put(@connected_client_pids, [client_pid | connected_client_pids])
              |> Map.put(@socket_process_map, new_socket_process_map)

            Logging.info("serving connection ~p:~p.", [client_ip_address, client_port])
            {:noreply, new_state}

          other ->
            Logging.warn(
              "could not start client process for connection from ~p:~p. cause of: ~p",
              [
                client_ip_address,
                client_port,
                other
              ]
            )

            :gen_tcp.close(client)
            {:noreply, state}
        end
    end
  end

  def handle_info(
        msg = {:EXIT, client_pid_in, reason},
        state = %{
          @connected_client_pids => connected_client_pids,
          @socket_process_map => socket_process_map,
          @disconnected_client_count => disconnected_client_count
        }
      ) do
    Logging.debug("an exit message arrived let check it. msg:~p", [msg])

    update_state = fn client ->
      client_pid = socket_process_map[client]

      new_connected_clients =
        connected_client_pids
        |> Enum.filter(fn x ->
          x != client_pid
        end)

      {_, new_socket_process_map} = socket_process_map |> Map.pop(client)

      new_state =
        state
        |> Map.put(@connected_client_pids, new_connected_clients)
        |> Map.put(@socket_process_map, new_socket_process_map)
        |> Map.put(@disconnected_client_count, disconnected_client_count + 1)

      {:noreply, new_state}
    end

    case client_pid_in do
      client_pid when is_pid(client_pid) ->
        case connected_client_pids |> Enum.member?(client_pid) do
          true ->
            keys =
              socket_process_map
              |> Enum.find(fn {key, val} -> val == client_pid end)

            if keys != nil do
              update_state.(keys |> elem(0))
            else
              Logging.debug("no socket found for this terminated process:~p. do nothing", [
                client_pid
              ])

              {:noreply, state}
            end

          false ->
            Logging.debug(
              "received exit from non-client process. let ignore it. reason:~p,pid:~p",
              [
                reason,
                client_pid
              ]
            )

            {:noreply, state}
        end

      client when is_port(client) ->
        update_state.(client)
    end
  end

  @impl true
  def terminate(
        reason,
        state = %{
          @listen_socket => listen_socket,
          @check_incoming_connection_tref => check_incoming_connection_tref,
          @ip_address => ip_address,
          @tcp_port => tcp_port
        }
      ) do
    Logging.info("server tcp listener for address ~p:~p terminating cause of :~p", [
      ip_address,
      tcp_port,
      reason
    ])

    :timer.cancel(check_incoming_connection_tref)
    :gen_tcp.close(listen_socket)
    state
  end
end
