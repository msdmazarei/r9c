defmodule EventLogger.Consumer.DruidPublisher do
  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.DurableQueue

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @druid_hosts "druid_hosts"
  @arrived_messages "arrived_messages"
  @processed_messages "processed_messages"
  @process_duration "process_duration"
  @last_arrived_message_batch_size "last_arrived_message_batch_size"
  @state_store_key "event_logger_consumer"
  @udpate_druid_config_timer "udpate_druid_config_interval"
  @last_druid_config_updated_time "last_druid_config_updated_time"

  def update_stats(state) do
    DatabaseEngine.Interface.LProcessData.set(
      {@state_store_key, self()},
      state
    )
  end

  def convert_to_druid_map(item) when is_map(item) do
    additional_data = Map.get(item, "additional_data") || %{}

    fin_map = %{
      "node" => :erlang.node(),
      "module" => Map.get(item, "module") || "",
      "function" => Map.get(item, "function") || "",
      "function_arity" => Map.get(item, "function_arity") || -1,
      "entity" => Map.get(item, "entity") || "",
      "eid" => Map.get(item, "eid") || "",
      "state" => Map.get(item, "state") || "",
      "additional_data" => Utilities.to_string(additional_data)
    }

    strings =
      :lists.seq(0, 9)
      |> Enum.map(fn x -> "idx_s#{x}" end)
      |> Enum.reduce(%{}, fn x, acc ->
        case Map.get(additional_data, x) do
          nil -> acc
          v -> acc |> Map.put(x, v)
        end
      end)

    integers =
      :lists.seq(0, 9)
      |> Enum.map(fn x -> "idx_i#{x}" end)
      |> Enum.reduce(%{}, fn x, acc ->
        case Map.get(additional_data, x) do
          nil -> acc
          v -> acc |> Map.put(x, v)
        end
      end)

    doubles =
      :lists.seq(0, 9)
      |> Enum.map(fn x -> "idx_d#{x}" end)
      |> Enum.reduce(%{}, fn x, acc ->
        case Map.get(additional_data, x) do
          nil -> acc
          v -> acc |> Map.put(x, v)
        end
      end)

    fin_map |> Map.merge(strings) |> Map.merge(integers) |> Map.merge(doubles)
  end

  def init(topic, partition) do
    Logging.debug("Called. topic:~p partition:~p", [topic, partition])
    druid_ingest_urls = DatabaseEngine.Interface.SystemConfig.Druid.IngestionURL.Repo.get_all()

    if length(druid_ingest_urls) == 0 do
      {:stop, "no druid host"}
    else
      {:ok,
       %{
         "topic" => topic,
         "partition" => partition,
         @arrived_messages => 0,
         @processed_messages => 0,
         @process_duration => 0,
         @druid_hosts => druid_ingest_urls,
         @last_arrived_message_batch_size => [],
         @udpate_druid_config_timer => 3 * 60_000,
         @last_druid_config_updated_time => Utilities.now()
       }}
    end
  end

  def handle_info(:update_config, state) do
    druid_ingest_urls = DatabaseEngine.Interface.SystemConfig.Druid.IngestionURL.Repo.get_all()

    if druid_ingest_urls |> length == 0 do
      {:stop, "no druid host", state}
    else
      new_state = state |> Map.put(@druid_hosts, druid_ingest_urls)
      {:noreply, new_state}
    end
  end

  def handle_message_set(
        message_set,
        state = %{
          "last_arrived_message_batch_size" => last_arrived_message_batch_size,
          @last_druid_config_updated_time => last_druid_config_updated_time,
          @udpate_druid_config_timer => udpate_druid_config_timer
        }
      ) do
    st = Utilities.now()

    state =
      if last_druid_config_updated_time > st + udpate_druid_config_timer do
        state =
          case handle_info(:update_config, state) do
            {:stop, v, _} -> throw(v)
            {:noreply, s} -> s
          end

        state |> Map.put(@last_druid_config_updated_time, Utilities.now())
      else
        state
      end

    Logging.debug("Called. message_set legnth:~p", [message_set |> length])

    decoded_messages_from_bin =
      message_set
      |> Enum.map(fn %Message{value: x} ->
        x |> DurableQueue.deserialize()
      end)


    druid_messages = decoded_messages_from_bin |> Enum.map(&convert_to_druid_map/1)
    
    

    new_state =
      state
      |> Map.put(
        @arrived_messages,
        state[@arrived_messages] + length(message_set)
      )

    update_stats(new_state)
    {:async_commit, new_state}
  end
end
