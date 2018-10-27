defmodule DatabaseEngine.DurableQueue.Consumers.SimpleLogConsumer do
  @moduledoc false

  use KafkaEx.GenConsumer

  alias KafkaEx.Protocol.Fetch.Message
  alias DatabaseEngine.DurableQueue

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  # note - messages are delivered in batches
  def handle_message_set(message_set, state) do
    KafkaEx.metadata()

    for %Message{value: message} <- message_set do
      Logging.debug(fn ->
        "Message:#{message} Deserialized: #{message |> DurableQueue.deserialize()}"
      end)
    end

    {:async_commit, state}
  end
end
