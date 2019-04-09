defmodule ProcessManager.Utils do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def stop_local_processes(process_state_filter_func) do
    to_stop =
      DatabaseEngine.Interface.LProcess.all_process_states()
      |> Enum.filter(fn {_, state} ->
        process_state_filter_func.(state)
      end)
      |> Enum.map(fn {pid, _} -> pid end)

    to_stop
    |> Enum.map(fn x ->
      GenServer.stop(x)
    end)

    :timer.sleep(1000)
  end

  def stop_processes(nodes, process_state_filter_func) do
    nodes
    |> Enum.map(fn nodename ->
      Task.async(fn ->
        call_result =
          :rpc.block_call(
            nodename,
            __MODULE__,
            :stop_local_processes,
            [process_state_filter_func],
            5_000
          )

        case call_result do
          {:badrpc, reason} ->
            Logging.error("prblem for node:~p cause of :~p", [nodename, reason])
            :ok

          _ ->
            :ok
        end
      end)
    end)
  end
end
