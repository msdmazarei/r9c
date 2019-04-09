defmodule Dispatcher.Process.Dispatching.Statistics do
  def total_stats() do
    DatabaseEngine.DurableQueue.get_consumers_stats()
    |> Enum.map(fn {qn, m} ->
      {qn, m["total"]}
    end)
    |> Map.new()
  end
end
