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

    {:async_commit, state}
  end
end
