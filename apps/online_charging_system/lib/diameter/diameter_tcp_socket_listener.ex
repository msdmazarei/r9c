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
  @spec init(map()) :: {:ok, %{optional(<<_::64, _::_*8>>) => any()}} | {:stop, any()}
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
        {i + (m["processed_packets"] || 0), o + (m["sent_packets"] || 0)}
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

  def print_breif(server_pid) do
    t1 =
      Task.async(fn ->
        GenServer.call(server_pid, :packets_stat)
      end)

    t2 =
      Task.async(fn ->
        Dispatcher.Process.Dispatching.Statistics.total_stats()
      end)

    t3 =
      Task.async(fn ->
        Dispatcher.Process.Statistics.total_statistic(Utilities.all_active_nodes())
      end)

    [t1r, t2r, t3r] =
      [t1, t2, t3]
      |> Enum.map(fn x ->
        Task.await(x)
      end)
      first_client = case t1r["per_client"]  do
        [] -> %{}
        v -> hd(v)
      end
    :io.fwrite(
      """
      PPT: Packet Processing Time ( create dia struct and push it)
      P_B2S_T: detect dia packets and push to mnesia time
      PIN: Total IN PACKET
      SRB: Socket Read Byte
      SRT: Socket Read Time
      PP: processed packets(enqueue)


      ~n~nTCPServer([SRB:~p, SRT:~p] [PIN:~p P_B2S_T:~p (RM:~p D:~p, M:~p) PPT:~p PP:~p]) ---> [A:(~p)] Dispatcher[processed:[P:(~p)] REQU:(~p) DRP:(~p)] [SD:(~p)] -----> [A:(~p), P:(~p)] Processes~n
      """,
      [
        first_client["received_bytes"],
        first_client["receive_byte_time_ms"],
        first_client["generated_packets"],
        first_client["bytes_to_struct_time_ms"],
        first_client["bytes_to_struct_time_ms_detection_part"],
        first_client["bytes_to_struct_time_ms_retrive_mnesia"],
        first_client["bytes_to_struct_time_ms_mnesia_part"],
        first_client["total_processing_packet_time"],
        first_client["processed_packets"],

        t2r["diameter_queue"]["arrived"],
        t2r["diameter_queue"]["processed"],
        t2r["diameter_queue"]["need_to_requeue_count"],
        t2r["diameter_queue"]["dropped_messages_cause_of_retry"],
        t2r["diameter_queue"]["successfully_delivered_to_uprocess"],

        t3r["total"]["arrived_messages"],
        t3r["total"]["processed_messages"]
      ]
    )

    :io.fwrite("PROCESS COUNT:~p~n", [t3r["total"]["process_count"]])

    :io.fwrite(" ~n TCPServer(~p)  <----- (~p) Processes~n", [
      t1r["out"],
      t3r["total"]["processed_messages"]
    ])
  end
end
