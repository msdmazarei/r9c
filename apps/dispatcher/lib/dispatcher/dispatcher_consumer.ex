defmodule Dispatcher.Consumers.InQConsumer do
  @moduledoc false

  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.DurableQueue

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  # note - messages are delivered in batches
  def handle_message_set(message_set, state) do
    Logging.debug("Called")

    message_set
    |> Enum.map(fn %Message{value: x} ->
      x |> DurableQueue.deserialize()
    end)
    # only messages will send to send_messages other will ignore
    |> Enum.filter(fn x ->
      case x do
        %DatabaseEngine.Models.SMS{} -> true
        %DatabaseEngine.Models.RadiusPacket{} -> true
        _ -> false
      end
    end)
    |> Dispatcher.Process.send_messages()

    {:async_commit, state}
  end
end
