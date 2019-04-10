defmodule Dispatcher.Process.Statistics do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def local_processes_state_of_type() do
    DatabaseEngine.Interface.LProcessData.all()|>Enum.map(&DatabaseEngine.Interface.LProcessData.get/1)
  end

  def local_total_statistic() do
    Logging.debug("called.")
    states = local_processes_state_of_type()

    r =
      states
      |> Enum.reduce(
        %{
          "arrived_messages" => 0,
          "processed_messages" => 0,
          "repeated_messages" => 0,
          "process_count" => 0
        },
        fn %{
             "processed_messages" => processed_messages,
             "arrived_messages" => arrived_messages,
             "repeated_messages" => repeated_messages
           },
           %{
             "arrived_messages" => t_arrived_messages,
             "processed_messages" => t_processed_messages,
             "process_count" => process_count,
             "repeated_messages" => t_repeated_messages
           } ->
          %{
            "arrived_messages" => t_arrived_messages + (processed_messages || 0),
            "processed_messages" => t_processed_messages + (arrived_messages || 0),
            "process_count" => process_count + 1,
            "repeated_messages" => t_repeated_messages + (repeated_messages || 0)
          }
        end
      )

    Logging.debug("rtn:~p", [r])
    r
  end

  # def debug_purpose_fn() do
  #   Dispatcher.Process.Statistics.local_total_statistic()
  # end

  def total_statistic(nodes) do
    tasks =
      nodes
      |> Enum.map(fn nodename ->
        Utilities.remote_async_task(
          nodename,
          Dispatcher.Process.Statistics,
          :local_total_statistic,
          []
        )
      end)

    results =
      Utilities.await_multi_task(tasks, 5_000, %{
        "arrived_messages" => 0,
        "processed_messages" => 0,
        "process_count" => 0,
        "repeated_messages" => 0
      })

    per_node_result = Enum.zip(nodes, results)
    Logging.debug("per_node_result:~p", [per_node_result])

    total =
      per_node_result
      |> Enum.reduce(
        %{
          "arrived_messages" => 0,
          "processed_messages" => 0,
          "process_count" => 0,
          "repeated_messages" => 0
        },
        fn {_, kv_map}, tkv_map ->
          kv_map
          |> Map.to_list()
          |> Enum.reduce(tkv_map, fn {k, v}, acc ->
            acc |> Map.put(k, acc[k] + (v || 0))
          end)
        end
      )

    %{
      "per_node_result" => per_node_result |> Map.new(),
      "total" => total
    }
  end
end
