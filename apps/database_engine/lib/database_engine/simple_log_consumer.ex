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

#      Process.sleep(10000)

    for %Message{value: message} <- message_set do
      Logging.debug("Message:#{message} Deserialized: #{message |> DurableQueue.deserialize()}")
    end

    {:async_commit, state}
  end
end
