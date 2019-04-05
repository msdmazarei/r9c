defmodule Dispatcher.Consumers.InQConsumer do
  @moduledoc false

  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.DurableQueue

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @arrived_messages "arrived_messages"
  @processed_messages "processed_messages"
  @process_duration "process_duration"
  @send_message_to_process_time "send_message_to_process_time"
  @process_creation_time "process_creation_time"
  @need_to_requeue_count "need_to_requeue_count"
  @dropped_messages_cause_of_retry "dropped_messages_cause_of_retry"
  @successfully_delivered_to_uprocess "successfully_delivered_to_uprocess"

  @requeue_retry_count 5

  def init(topic, partition) do
    Logging.debug("Called. topic:~p partition:~p", [topic, partition])

    {:ok,
     %{
       "topic" => topic,
       "partition" => partition,
       @arrived_messages => 0,
       @processed_messages => 0
     }}
  end

  # note - messages are delivered in batches
  def handle_message_set(message_set, state) do
    st = Utilities.now()

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

    dropped_messages_cause_of_retry =
      need_to_requeue
      |> Enum.filter(fn msg ->
        (msg.options["__requeue_retry__"] || 0) >= @requeue_retry_count - 1
      end)
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

    need_to_requeue
    |> Enum.map(fn msg ->
      :ok = DatabaseEngine.DurableQueue.enqueue(state["topic"], state["partition"], msg)
    end)

    du = Utilities.now() - st + (state[@process_duration] || 0)

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

    {:async_commit, new_state}
  end
end
