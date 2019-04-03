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
    Dispatcher.Process.send_messages(to_send_message)
    processed_messages = (state[@processed_messages] || 0) + length(to_send_message)
    arrived_messages = (state[@arrived_messages] || 0) + length(message_set)

    du = Utilities.now() - st + (state[@process_duration] || 0)

    new_state =
      state
      |> Map.put(@processed_messages, processed_messages)
      |> Map.put(@arrived_messages, arrived_messages)
      |> Map.put(@process_duration, du)

    {:async_commit, new_state}
  end
end
