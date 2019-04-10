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
  @stats_drop_packets "stats_drop_packets"
  @stats_enqueue_packets "stats_enqueue_packets"
  @stats_reprocess_packets "stats_reprocess_packets"
  @process_name "process_name"
  @output_buffer_count "output_buffer_count"
  @tcpreader_pid "tcpreader_pid"
  @tcpwriter_pid "tcpwriter_pid"
  @bytes_convertor_pid "bytes_convertor_pid"
  @pkt_processor_pid "pkt_processor_pid"
  @last_arrived_message_timestamp "last_arrived_message_timestamp"
  @stats "stats"
  @impl true
  def init(arg = %{"client" => client_socket, "config" => client_configs}) do
    Logging.debug("Calld.")
    output_buffer_count = arg["output_buffer_count"] || 10

    {:ok, {client_ip_tuple, client_port}} = :inet.peername(client_socket)
    client_ip_address_string = Utilities.Conversion.ip_address_tuple_to_string(client_ip_tuple)
    {:ok, tref_agg_stats} = :timer.send_interval(1000, :agg_stats)
    Logging.debug("return started successfully")
    now = Utilities.now()
    pname = "#{now};diameter_client::#{client_ip_address_string}:#{client_port}"
    pname = String.to_atom(pname)
    Process.register(self(), pname)

    outbuffers =
      :lists.seq(0, output_buffer_count - 1)
      |> Enum.map(fn buf_no ->
        "out_buffer_#{pname}_#{buf_no}"
      end)

    mnesia_in_buffer_key = "input_buffer_#{pname}"

    tcpreader_pid =
      spawn_link(Networking.TcpReaderToMnesiaBuffer, :loop, [
        %{
          "socket" => client_socket,
          "mnesia_buffer_key" => mnesia_in_buffer_key,
          "master_pid" => self(),
          "received_bytes" => 0
        }
      ])

    tcpwriter_pid =
      spawn_link(Networking.TcpWriterFromMnesiaBuffer, :loop, [
        %{
          "socket" => client_socket,
          "mnesia_buffer_keys" => outbuffers,
          "sent_bytes" => 0,
          "sent_packets" => 0,
          "master_pid" => self(),
          "client_address" => "#{client_ip_address_string}:#{client_port}"
        }
      ])

    bytes_convertor_pid =
      spawn_link(
        OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess.ConvertBytesToStructLoop,
        :loop,
        [
          %{
            "mnesia_buffer_key" => mnesia_in_buffer_key,
            "mnesia_packets_key" => "input_packet_#{pname}",
            "master_pid" => self(),
            "processed_bytes" => 0,
            "generated_packets" => 0
          }
        ]
      )

    pkt_processor_pid =
      spawn_link(
        OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess.ProcessInPacketsLoop,
        :loop,
        [
          %{
            "mnesia_packets_key" => "input_packet_#{pname}",
            "master_pid" => self(),
            "processed_packets" => 0,
            "droped_packets" => 0,
            "requeued_packets" => 0,
            "client_config" => client_configs,
            "client_address" => "#{client_ip_address_string}:#{client_port}",
            "client_ip" => client_ip_address_string,
            "client_port" => client_port,
            "pname" => pname,
            "output_buffer_count" => output_buffer_count,
            "per_q_success_failuar_map" => %{}
          }
        ]
      )

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
       @output_buffer_count => output_buffer_count,
       @trefs => [],
       @stats_in_packets => 0,
       @stats_out_packets => 0,
       @stats_drop_packets => 0,
       @stats_enqueue_packets => 0,
       @stats_reprocess_packets => 0,
       @tcpreader_pid => tcpreader_pid,
       @tcpwriter_pid => tcpwriter_pid,
       @bytes_convertor_pid => bytes_convertor_pid,
       @pkt_processor_pid => pkt_processor_pid,
       @stats => %{}
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
        :agg_stats,
        state = %{
          @tcpreader_pid => tcpreader_pid,
          @tcpwriter_pid => tcpwriter_pid,
          @bytes_convertor_pid => bytes_convertor_pid,
          @pkt_processor_pid => pkt_processor_pid
        }
      ) do
    processes = [tcpreader_pid, tcpwriter_pid, bytes_convertor_pid, pkt_processor_pid]

    processes
    |> Enum.map(fn x ->
      send(x, {self(), :set_stats, :stats})
    end)

    {:noreply, state}
  end

  def handle_info({:set_stats, inc}, state) do
    stats =
      inc
      |> Map.to_list()
      |> Enum.reduce(state[@stats], fn {k, v}, acc ->
        acc |> Map.put(k, v)
      end)

    new_state = state |> Map.put(@stats, stats)

    {:noreply, new_state}
  end

  def handle_info({:update_stats, inc}, state) do
    stats =
      inc
      |> Map.to_list()
      |> Enum.reduce(state[@stats], fn {k, v}, acc ->
        acc |> Map.put(k, (acc[k] || 0) + v)
      end)

    new_state = state |> Map.put(@stats, stats)

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

  def release_resources(
        _state = %{
          @client => client,
          @trefs => trefs,
          @process_name => pname,
          @output_buffer_count => output_buffer_count
        }
      ) do
    trefs
    |> Enum.map(fn x ->
      :timer.cancel(x)
    end)

    out_buffers =
      :lists.seq(0, output_buffer_count - 1)
      |> Enum.map(fn buf_no ->
        "out_buffer_#{pname}_#{buf_no}"
      end)

    keys =
      [
        "input_packet_#{pname}",
        "input_buffer_#{pname}",
        "process_received_packets_#{pname}",
        "process_bytes_lock_#{pname}"
      ] ++ out_buffers

    keys |> Enum.map(&DatabaseEngine.Interface.LKV.del/1)

    :gen_tcp.close(client)
  end

  def transfer_local_buffers_to_main_node_buffers(
        main_node,
        process_name,
        output_buffer_count,
        transferer_state
      ) do
    :timer.sleep(100)

    outbuffers =
      :lists.seq(0, output_buffer_count - 1)
      |> Enum.map(fn bn ->
        fetch_and_empty_output_buffer(
          %{@process_name => process_name, @output_buffer_count => output_buffer_count},
          bn
        )
      end)

    {packet_bytes, packet_count} =
      outbuffers
      |> Enum.reduce({<<>>, 0}, fn {b, c}, {tb, tc} ->
        # Logging.debug("b:~p c:~p tb:~p tc:~p",[b,c,tb,tc])
        {<<tb::binary, b::binary>>, tc + c}
      end)

    transferer_state =
      if packet_count > 0 do
        Logging.debug("transfering packet:~p byts:~p", [
          packet_count,
          byte_size(packet_bytes)
        ])

        r =
          :rpc.call(
            main_node,
            __MODULE__,
            :append_to_output_buffer,
            [
              %{@process_name => process_name, @output_buffer_count => output_buffer_count},
              packet_bytes,
              packet_count
            ],
            5000
          )

        case r do
          {:badrpc, reason} ->
            Logging.warn("problem to send back result to main target node:~p. cause of :~p", [
              main_node,
              reason
            ])

            transferer_state

          _ ->
            Logging.debug("successfully buffers sent back to main node:~p", [main_node])
            transferer_state |> Map.put("last_sent_epoch", Utilities.now())
        end
      else
        transferer_state
      end

    if Utilities.now() > (transferer_state["last_sent_epoch"] || 0) + 60_000 do
      # that is 60 seconds without any data to send back to main node
      Logging.debug(
        "exiting from transferer process cause of no data exists to send for 1 minute"
      )

      result =
        DatabaseEngine.Interface.LKV.transaction(fn ->
          :lists.seq(0, output_buffer_count - 1)
          |> Enum.map(fn x ->
            del_out_buffer_if_its_empty(%{@process_name => process_name}, x)
          end)
        end)

      case result do
        {:aborted, _} ->
          Logging.debug("find a new buffer has value. dont kill this process.")

          transferer_state = transferer_state |> Map.put("last_sent_epoch", Utilities.now())

          transfer_local_buffers_to_main_node_buffers(
            main_node,
            process_name,
            output_buffer_count,
            transferer_state
          )

        _ ->
          Logging.debug("process is going to terminate")
          :ok
      end
    else
      transfer_local_buffers_to_main_node_buffers(
        main_node,
        process_name,
        output_buffer_count,
        transferer_state
      )
    end
  end

  def start_if_not_already_started_transfer_process(
        main_node,
        process_name,
        output_buffer_count
      ) do
    Logging.debug("Called.")
    transfer_process_name = "buffer_transferer_#{process_name}" |> String.to_atom()
    transfer_pid = :erlang.whereis(transfer_process_name)
    Logging.debug("transfer_process_name:~p", [transfer_process_name])

    if is_pid(transfer_pid) do
      transfer_pid
    else
      transfer_pid =
        spawn(__MODULE__, :transfer_local_buffers_to_main_node_buffers, [
          main_node,
          process_name,
          output_buffer_count,
          %{}
        ])

      :erlang.register(transfer_process_name, transfer_pid)
      Logging.debug("spawned pid:~p name:~p", [transfer_pid, transfer_process_name])
      transfer_pid
    end
  end

  def send_response_back_to_client({n, pn, output_buffer_count}, script_state) do
    Logging.debug("Called. cun: ~p r-pid:~p scr:~p", [
      node(),
      {n, pn},
      script_state.last_cel_result
    ])

    case script_state.last_cel_result do
      {:return, [dia_packet = %Utilities.Parsers.Diameter.DiameterPacket{}]} ->
        # Logging.info("send dia_packet to process:~p", [pid])
        # GenServer.call(pid,{:send_diameter_packet, dia_packet})
        # r=:rpc.call(pid|>node, GenServer, :call, [pid,{:send_diameter_packet, dia_packet}],5000)
        # Logging.info("result rpc call:~p")
        bytes = dia_packet |> Utilities.Parsers.Diameter.serialize_to_bin()
        Logging.debug("append_to_output_buffer ing...")

        append_to_output_buffer(
          %{@process_name => pn, @output_buffer_count => output_buffer_count},
          bytes,
          1
        )

        Logging.debug("appended")

        case node() do
          ^n ->
            # we are on target node which real buffers are here
            Logging.debug("we are on main node")
            :ok

          _ ->
            # we are on remote node
            Logging.debug("we are not on main node")

            start_if_not_already_started_transfer_process(n, pn, output_buffer_count)
        end

      v ->
        Logging.warn("unexpected result from script: ~p", [v])
    end
  end

  def detect_diameter_packet(buf, diameters \\ []) do
    # Logging.debug("called. with buf-len:~p, dia-len:~p", [
    #   byte_size(buf || <<>>),
    #   length(diameters)
    # ])

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

  def transactionally_do(func) do
    case :mnesia.is_transaction() do
      true ->
        func.()

      false ->
        case :mnesia.transaction(func) do
          {:atomic, v} ->
            v

          {:aborted, err} ->
            Logging.warn("problem to stroe to local tb LKV. cause:~p", [err])
            false
        end
    end
  end

  @spec once_fetch_and_empty_output_buffer(map(), any()) :: any()
  def once_fetch_and_empty_output_buffer(
        _state = %{@process_name => pname},
        buf_no
      ) do
    # Logging.debug("called. buf_no:~p, pname:~p", [buf_no, pname])
    key = "out_buffer_#{pname}_#{buf_no}"
    # Logging.debug("out buffer key:~p", [key])

    r =
      :mnesia.transaction(
        fn ->
          {old_bys, old_count} =
            case DatabaseEngine.Interface.LKV.get_for_update(key) do
              {b, c} when is_binary(b) and is_number(c) -> {b, c}
              _ -> {<<>>, 0}
            end

          new_bys = <<>>
          DatabaseEngine.Interface.LKV.set(key, {new_bys, 0})
          {old_bys, old_count}
        end,
        [],
        1
      )

    case r do
      {:atomic, v} -> v
      {:aborted, _} -> {<<>>, 0}
      {v, c} when is_binary(v) -> {v, c}
    end
  end

  def del_out_buffer_if_its_empty(
        _state = %{@process_name => pname},
        buf_no
      ) do
    key = "out_buffer_#{pname}_#{buf_no}"

    transactionally_do(fn ->
      case DatabaseEngine.Interface.LKV.get_for_update(key) do
        {_, n} when n > 0 ->
          Logging.debug("buffer :~p has value n is :~p",[buf_no, n])
          :mnesia.abort("has value")

        _ ->
          Logging.debug("buf :~p deleted",[buf_no])
          DatabaseEngine.Interface.LKV.del(key)
      end
    end)
  end

  def fetch_and_empty_output_buffer(
        _state = %{@process_name => pname},
        buf_no
      ) do
    # Logging.debug("called. buf_no:~p, pname:~p", [buf_no, pname])
    key = "out_buffer_#{pname}_#{buf_no}"
    # Logging.debug("out buffer key:~p", [key])

    r =
      transactionally_do(fn ->
        {old_bys, old_count} =
          case DatabaseEngine.Interface.LKV.get_for_update(key) do
            {b, c} when is_binary(b) and is_number(c) -> {b, c}
            _ -> {<<>>, 0}
          end

        new_bys = <<>>
        DatabaseEngine.Interface.LKV.set(key, {new_bys, 0})
        {old_bys, old_count}
      end)

    case r do
      {:atomic, v} -> v
      {:aborted, _} -> {<<>>, 0}
      {v, c} when is_binary(v) -> {v, c}
    end
  end

  def append_to_output_buffer(
        _state = %{@process_name => pname, @output_buffer_count => output_buffer_count},
        bytes,
        pkt_count
      ) do
    buf_no = rem(Utilities.randint(output_buffer_count), output_buffer_count)
    key = "out_buffer_#{pname}_#{buf_no}"
    # Logging.debug("output_buffer: ~p", [key])

    transactionally_do(fn ->
      {old_bys, old_count} = DatabaseEngine.Interface.LKV.get_for_update(key) || {<<>>, 0}
      new_bys = <<old_bys::binary, bytes::binary>>
      new_count = old_count + pkt_count
      DatabaseEngine.Interface.LKV.set(key, {new_bys, new_count})
    end)
  end

  def set_input_packets(_state = %{@process_name => pname}, value) do
    key = "input_packet_#{pname}"

    transactionally_do(fn ->
      DatabaseEngine.Interface.LKV.set(key, value)
    end)
  end

  def get_input_packets(_state = %{@process_name => pname}) do
    key = "input_packet_#{pname}"
    DatabaseEngine.Interface.LKV.get(key) || []
  end

  def get_input_packets_for_update(_state = %{@process_name => pname}) do
    key = "input_packet_#{pname}"
    DatabaseEngine.Interface.LKV.get_for_update(key) || []
  end

  def append_input_packets(_state = %{@process_name => pname}, new_packets)
      when is_list(new_packets) do
    key = "input_packet_#{pname}"

    transactionally_do(fn ->
      old_packets = DatabaseEngine.Interface.LKV.get_for_update(key) || []
      packets = old_packets ++ new_packets
      DatabaseEngine.Interface.LKV.set(key, packets)
    end)
  end

  def set_input_buffer(_state = %{@process_name => pname}, new_bytes) do
    key = "input_buffer_#{pname}"

    transactionally_do(fn ->
      DatabaseEngine.Interface.LKV.set(key, new_bytes)
    end)
  end

  def get_input_buffer_for_update(_state = %{@process_name => pname}) do
    key = "input_buffer_#{pname}"
    DatabaseEngine.Interface.LKV.get_for_update(key) || <<>>
  end

  def get_input_buffer(_state = %{@process_name => pname}) do
    key = "input_buffer_#{pname}"
    DatabaseEngine.Interface.LKV.get(key) || <<>>
  end

  def append_to_input_buffer(
        _state = %{@process_name => pname},
        new_bytes
      ) do
    transactionally_do(fn ->
      key = "input_buffer_#{pname}"
      buf = DatabaseEngine.Interface.LKV.get_for_update(key) || <<>>
      new_buf = <<buf::binary, new_bytes::binary>>

      DatabaseEngine.Interface.LKV.set(key, new_buf)
      byte_size(new_bytes)
    end)
  end

  def read_from_tcp_socket(client, state) do
    case :gen_tcp.recv(client, 0, 0) do
      {:error, :timeout} ->
        0

      {:error, reason} ->
        Logging.debug("problem in client socket reading. reason:~p. kill me", [reason])
        {:terminate, reason}

      # Process.exit(self()ormal)

      {:ok, packet} ->
        Logging.debug("new packet arrived. it is packet-length:~p", [byte_size(packet)])
        append_to_input_buffer(state, packet)


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
