defmodule Dispatcher.Consumers.InQConsumer do
  @moduledoc false

  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.DurableQueue

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Utils.EventLogger
  alias DatabaseEngine.Utils.EventLogger

  @arrived_messages "arrived_messages"
  @processed_messages "processed_messages"
  @process_duration "process_duration"
  @send_message_to_process_time "send_message_to_process_time"
  @process_creation_time "process_creation_time"
  @need_to_requeue_count "need_to_requeue_count"
  @dropped_messages_cause_of_retry "dropped_messages_cause_of_retry"
  @successfully_delivered_to_uprocess "successfully_delivered_to_uprocess"
  @last_arrived_message_batch_size "last_arrived_message_batch_size"

  @requeue_retry_count 5

  def update_stats(state) do
    DatabaseEngine.Interface.LProcessData.set(
      {"consumer", self()},
      state
    )
  end

  def init(topic, partition) do
    Logging.debug("Called. topic:~p partition:~p", [topic, partition])

    rtn =
      {:ok,
       %{
         "topic" => topic,
         "partition" => partition,
         @arrived_messages => 0,
         @processed_messages => 0,
         @last_arrived_message_batch_size => []
       }}

    EventLogger.log_event("", "", "start", %{
      "topic" => topic,
      "partition" => partition,
      "__index" => %{
        "s1" => topic,
        "s2" => Utilities.to_string(partition)
      }
    })

    rtn
  end

  # note - messages are delivered in batches

  def handle_info({:EXIT, _, :normal}, state) do
    # sometimes cause of Task.async we receive this messages from child processes
    # we need this to avoid log error in log file cause of nothing
    {:noreply, state}
  end

  def speed_meter(
        message_set,
        state = %{"last_arrived_message_batch_size" => last_arrived_message_batch_size}
      ) do
    st = Utilities.now()

    Logging.debug("Called. message_set legth:~p", [message_set |> length])

    processed_messages = (state[@processed_messages] || 0) + length(message_set)
    arrived_messages = (state[@arrived_messages] || 0) + length(message_set)
    process_creation_time = (state[@process_creation_time] || 0) + 0

    send_message_to_process_time = (state[@send_message_to_process_time] || 0) + 0

    need_to_requeue_count = 0

    dropped_messages_cause_of_retry = 0

    last_arrived_message_batch_size =
      case last_arrived_message_batch_size do
        l when length(l) > 100 ->
          [length(message_set) | Enum.slice(l, 0, 80)]

        l ->
          [length(message_set) | l]
      end

    new_state =
      state
      |> Map.put(@processed_messages, processed_messages)
      |> Map.put(@arrived_messages, arrived_messages)
      |> Map.put(@process_creation_time, process_creation_time)
      |> Map.put(@send_message_to_process_time, send_message_to_process_time)
      |> Map.put(@need_to_requeue_count, need_to_requeue_count)
      |> Map.put(
        @successfully_delivered_to_uprocess,
        processed_messages
      )
      |> Map.put(
        @dropped_messages_cause_of_retry,
        0
      )
      |> Map.put(@process_duration, 0)
      |> Map.put(@last_arrived_message_batch_size, last_arrived_message_batch_size)

    {:async_commit, new_state}
  end

  def handle_message_set__(message_set, state) do
    speed_meter(message_set, state)
  end

  def log_event_entity(main_state, entity_name, entity_id, state, data) do
    index_map = data["__index"] || %{}

    index_map =
      index_map
      |> Map.put("s1", main_state["topic"])
      |> Map.put("s2", Utilities.to_string(main_state["partition"]))

    data = data |> Map.put("__index", index_map)

    %{
      "time" => Utilities.now(),
      "entity_name" => entity_name,
      "entity_id" => entity_id,
      "state" => state,
      "additional_data" => data
    }
  end

  def filter_by_type(list, type) do
    list
    |> Enum.filter(fn x ->
      x.__struct__ == type
    end)
  end

  def count_entity_log(main_state, entity, count, state) do
    if count > 0 do
      log_event_entity(main_state, entity |> Utilities.to_string(), "", state, %{
        "__index" => %{"i0" => count}
      })
    else
      nil
    end
  end

  def update_events(event_logs, state, list_of_msgs, type) do
    smses = list_of_msgs |> filter_by_type(DatabaseEngine.Models.SMS)
    sms_count = smses |> length

    event_logs = [
      count_entity_log(state, DatabaseEngine.Models.SMS, sms_count, "#{type}_count") | event_logs
    ]

    event_logs =
      if(sms_count > 0) do
        arrive_sms_ids =
          smses
          |> Enum.map(fn x ->
            log_event_entity(
              state,
              DatabaseEngine.Models.SMS |> Utilities.to_string(),
              x.id,
              type,
              %{}
            )
          end)

        event_logs ++ arrive_sms_ids
      else
        event_logs
      end

    radiuses = list_of_msgs |> filter_by_type(DatabaseEngine.Models.RadiusPacket)
    radius_count = radiuses |> length

    event_logs = [
      count_entity_log(state, DatabaseEngine.Models.RadiusPacket, radius_count, type)
      | event_logs
    ]

    event_logs =
      if(radius_count > 0) do
        arrive_radius_ids =
          radiuses
          |> Enum.map(fn x ->
            log_event_entity(
              state,
              DatabaseEngine.Models.RadiusPacket |> Utilities.to_string(),
              x.id,
              type,
              %{}
            )
          end)

        event_logs ++ arrive_radius_ids
      else
        event_logs
      end

    diameters = list_of_msgs |> filter_by_type(DatabaseEngine.Models.DiameterPacket)
    diameter_count = diameters |> length

    event_logs = [
      count_entity_log(
        state,
        DatabaseEngine.Models.DiameterPacket,
        diameter_count,
        type
      )
      | event_logs
    ]

    event_logs =
      if(diameter_count > 0) do
        arrive_diameter_ids =
          diameters
          |> Enum.map(fn x ->
            log_event_entity(
              state,
              DatabaseEngine.Models.DiameterPacket |> Utilities.to_string(),
              x.id,
              type,
              %{}
            )
          end)

        event_logs ++ arrive_diameter_ids
      else
        event_logs
      end

    event_logs
  end

  def handle_message_set(
        message_set,
        state = %{"last_arrived_message_batch_size" => last_arrived_message_batch_size}
      ) do
    st = Utilities.now()
    event_logs = []

    Logging.debug("Called. message_set legth:~p", [message_set |> length])

    to_send_message =
      message_set
      |> Enum.map(fn %Message{value: x} ->
        x |> DurableQueue.deserialize()
      end)
      # only messages will send to send_messages other will ignore
      |> Enum.filter(fn x ->
        case x do
          %DatabaseEngine.Models.SMS{} -> true
          %DatabaseEngine.Models.RadiusPacket{} -> true
          %DatabaseEngine.Models.DiameterPacket{} -> true
          _ -> false
        end
      end)

    event_logs = update_events(event_logs, state, to_send_message, "arrive")

    Logging.debug("call send messages by messages len:~p", [to_send_message |> length])

    %{
      "stats" => %{
        "send_message_to_process_time" => send_message_to_process_time,
        "process_creation_time" => process_creation_time
      },
      "need_to_requeue" => need_to_requeue
    } = Dispatcher.Process.send_messages(to_send_message)

    processed_messages = (state[@processed_messages] || 0) + length(to_send_message)
    arrived_messages = (state[@arrived_messages] || 0) + length(message_set)
    process_creation_time = (state[@process_creation_time] || 0) + process_creation_time

    send_message_to_process_time =
      (state[@send_message_to_process_time] || 0) + send_message_to_process_time

    need_to_requeue_count = (state[@need_to_requeue_count] || 0) + length(need_to_requeue || [])

    dropable_messages =
      need_to_requeue
      |> Enum.filter(fn msg ->
        (msg.options["__requeue_retry__"] || 0) >= @requeue_retry_count - 1
      end)

    event_logs = update_events(event_logs, state, dropable_messages, "drop")

    dropped_messages_cause_of_retry =
      dropable_messages
      |> length

    need_to_requeue =
      need_to_requeue
      |> Enum.map(fn msg ->
        new_option =
          case msg.options do
            nil -> %{}
            %{} -> msg.options
            _ -> %{}
          end

        retry_count = new_option["__requeue_retry__"] || 0
        new_option = new_option |> Map.put("__requeue_retry__", retry_count + 1)
        msg = %{msg | options: new_option}
        msg
      end)
      |> Enum.filter(fn x ->
        x.options["__requeue_retry__"] < @requeue_retry_count
      end)

    event_logs = update_events(event_logs, state, need_to_requeue, "requeue")

    need_to_requeue
    |> Enum.map(fn msg ->
      :ok = DatabaseEngine.DurableQueue.enqueue(state["topic"], state["partition"], msg)
    end)

    du = Utilities.now() - st + (state[@process_duration] || 0)

    last_arrived_message_batch_size =
      case last_arrived_message_batch_size do
        l when length(l) > 100 ->
          [length(message_set) | Enum.slice(l, 0, 80)]

        l ->
          [length(message_set) | l]
      end

    event_logs = event_logs |> Enum.filter(fn x -> x != nil end)
    EventLogger.log_events(event_logs)

    new_state =
      state
      |> Map.put(@processed_messages, processed_messages)
      |> Map.put(@arrived_messages, arrived_messages)
      |> Map.put(@process_creation_time, process_creation_time)
      |> Map.put(@send_message_to_process_time, send_message_to_process_time)
      |> Map.put(@need_to_requeue_count, need_to_requeue_count)
      |> Map.put(
        @successfully_delivered_to_uprocess,
        (state[@successfully_delivered_to_uprocess] || 0) +
          (length(message_set) - dropped_messages_cause_of_retry - length(need_to_requeue))
      )
      |> Map.put(
        @dropped_messages_cause_of_retry,
        (state[@dropped_messages_cause_of_retry] || 0) + dropped_messages_cause_of_retry
      )
      |> Map.put(@process_duration, du)
      |> Map.put(@last_arrived_message_batch_size, last_arrived_message_batch_size)

    update_stats(new_state)

    {:async_commit, new_state}
  end
end
