defmodule Networking.TcpWriterFromMnesiaBuffer do
  @socket "socket"
  @mnesia_buffer_keys "mnesia_buffer_keys"
  @sent_bytes "sent_bytes"
  @sent_packets "sent_packets"
  @master_pid "master_pid"
  @client_address "client_address"
  @terminate_reason "terminate_reason"

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.LKV
  alias DatabaseEngine.Interface.LKV

  def once_fetch_and_empty_output_buffer(key) do
    r =
      :mnesia.transaction(
        fn ->
          {old_bys, old_count} =
            case LKV.get_for_update(key) do
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
    end
  end

  def send_to_socket(client, client_address, buffers) do
    bys_pkts_count =
      buffers
      |> Enum.map(fn bfk ->
        {bys, pkt_count} = once_fetch_and_empty_output_buffer(bfk) || {<<>>, 0}

        if byte_size(bys) > 0 do
          Logging.debug("flushing buf: ~p bysize:~p", [bfk, byte_size(bys)])

          case :gen_tcp.send(client, bys) do
            :ok ->
              Logging.debug("buffer with len:~p sent successfully to client:~p", [
                byte_size(bys),
                client_address
              ])

              {byte_size(bys), pkt_count}

            {:error, reason} ->
              Logging.debug("Problem to send buffer to client:~p cause of ~p", [
                client_address,
                reason
              ])

              {:terminate, reason}
          end
        else
          {0, 0}
        end
      end)

    bys_pkts_count
    |> Enum.reduce({0, 0}, fn {b, p}, {tb, tp} ->
      case tb do
        :terminate -> {tb, tp}
        _ -> {tb + b, tp + p}
      end
    end)
  end

  def loop(
        state = %{
          @socket => socket,
          @client_address => client_address,
          @mnesia_buffer_keys => mnesia_buffer_keys,
          @sent_bytes => sent_bytes,
          @sent_packets => sent_packets
        }
      ) do
    {continue, state} =
      receive do
        income_msg ->
          {continue, state} =
            case income_msg do
              {sender, back_reference, :stats} ->
                send(
                  sender,
                  {back_reference, %{@sent_bytes => sent_bytes, @sent_packets => sent_packets}}
                )

                {true, state}

              {:EXIT, reason} ->
                Logging.debug("Exit Received. cause of: ~p",[reason])
                state |> Map.put(@terminate_reason, {:EXIT, reason})
                {false, state}

              _ ->
                {true, state}
            end

          {continue, state}
          # code
      after
        0 ->
          case send_to_socket(socket, client_address, mnesia_buffer_keys) do
            {:terminate, reason} ->
              state = state |> Map.put(@terminate_reason, reason)
              {false, state}

            {tb, tp} when is_number(tb) ->
              state =
                if tb == 0 do
                  :timer.sleep(20)
                  state
                else
                  state
                  |> Map.put(@sent_bytes, sent_bytes + tb)
                  |> Map.put(@sent_packets, sent_packets + tp)
                end

              {true, state}
          end
      end

    if continue do
      loop(state)
    else
      Process.exit(self(), state[@terminate_reason])
    end
  end
end
