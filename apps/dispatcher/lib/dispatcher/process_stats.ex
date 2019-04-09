defmodule Dispatcher.Process.Statistics do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def local_processes_state_of_type() do
    DatabaseEngine.Interface.LProcess.all_process_states()
    |> Enum.map(fn {_, s} -> s end)

    # |> Enum.filter(fn x ->
    #   filter_func.(x)
    # end)
  end

  def statistics_of_process(state) do
    %{
      "processed_messages" => state.processed_messages,
      "arrived_messages" => state.arrived_messages,
      "last_arrived_message_time" => state.last_arrived_message_time
    }
  end

  def local_total_statistic() do
    Logging.debug("called.")
    states = local_processes_state_of_type()

    r =
      states
      |> Enum.map(&statistics_of_process/1)
      |> Enum.reduce(
        %{
          "arrived_messages" => 0,
          "processed_messages" => 0,
          "process_count" => 0
        },
        fn %{
             "processed_messages" => processed_messages,
             "arrived_messages" => arrived_messages
           },
           %{
             "arrived_messages" => t_arrived_messages,
             "processed_messages" => t_processed_messages,
             "process_count" => process_count
           } ->
          %{
            "arrived_messages" => t_arrived_messages + (processed_messages || 0),
            "processed_messages" => t_processed_messages + (arrived_messages || 0),
            "process_count" => process_count + 1
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
        Utilities.remote_async_task(nodename, fn ->
           Dispatcher.Process.Statistics.local_total_statistic()
        end)
      end)

    results =
      Utilities.await_multi_task(tasks, 5_000, %{
        "arrived_messages" => 0,
        "processed_messages" => 0,
        "process_count" => 0
      })

    #   Task.async(fn ->
    #     case :rpc.call(
    #            nodename,
    #            __MODULE__,
    #            :local_total_statistic,
    #            [process_state_filter_func],
    #            5_000
    #          ) do
    #       {:badrpc, reason} ->
    #         Logging.error("problem to retrive process statistics for node:~p. reason: ~p", [
    #           nodename,
    #           reason
    #         ])

    #         {nodename,
    #          %{
    #            "arrived_messages" => 0,
    #            "processed_messages" => 0,
    #            "process_count" => 0
    #          }}

    #       v ->
    #         {nodename, v}
    #     end
    #   end)
    # end)

    per_node_result = Enum.zip(nodes, results)
    Logging.debug("per_node_result:~p", [per_node_result])
    # tasks
    # |> Enum.map(fn t ->
    #   Task.await(t)
    # end)

    total =
      per_node_result
      |> Enum.reduce(
        %{
          "arrived_messages" => 0,
          "processed_messages" => 0,
          "process_count" => 0
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