defmodule Networking.TcpReaderToMnesiaBuffer do
  @socket "socket"
  @mnesia_buffer_key "mnesia_buffer_key"
  @received_bytes "received_bytes"
  @receive_byte_time_ms "receive_byte_time_ms"
  @master_pid "master_pid"
  @terminate_reason "terminate_reason"

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.LKV
  alias DatabaseEngine.Interface.LKV

  def append_to_input_buffer(
        _state = %{@mnesia_buffer_key => key},
        new_bytes
      ) do
    LKV.transaction(fn ->
      buf = LKV.get_for_update(key) || <<>>
      new_buf = <<buf::binary, new_bytes::binary>>

      LKV.set(key, new_buf)
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

      {:ok, packet} ->
        Logging.debug("new packet arrived. it is packet-length:~p", [byte_size(packet)])

        case append_to_input_buffer(state, packet) do
          {:atomic, n} when is_number(n) -> n
          {:aborted, reason} -> {:terminate, reason}
        end
    end
  end

  def loop(
        state = %{
          @socket => socket,
          @received_bytes => received_bytes
        }
      ) do
    received_bytes = received_bytes || 0
    receive_byte_time_ms = state[@receive_byte_time_ms] || 0

    {continue, state} =
      receive do
        income_msg ->
          {continue, state} =
            case income_msg do
              {sender, back_reference, :stats} ->
                send(
                  sender,
                  {back_reference,
                   %{
                     @received_bytes => received_bytes,
                     @receive_byte_time_ms => receive_byte_time_ms
                   }}
                )

                {true, state}

              {:EXIT, reason} ->
                Logging.debug("Exit Received. cause of: ~p", [reason])
                state |> Map.put(@terminate_reason, {:EXIT, reason})
                {false, state}

              _ ->
                {true, state}
            end

          {continue, state}
          # code
      after
        0 ->
          st_read = Utilities.now()

          case read_from_tcp_socket(socket, state) do
            {:terminate, reason} ->
              state = state |> Map.put(@terminate_reason, reason)
              {false, state}

            n when is_number(n) ->
              state =
                if n == 0 do
                  :timer.sleep(20)
                  state
                else
                  du_read = Utilities.now() - st_read

                  state
                  |> Map.put(@received_bytes, received_bytes + n)
                  |> Map.put(@receive_byte_time_ms, du_read + receive_byte_time_ms)
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
