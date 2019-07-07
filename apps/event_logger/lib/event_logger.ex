defmodule EventLogger do
  @moduledoc """
  Documentation for EventLogger.
  """

  @doc """
  Hello world.

  ## Examples

      iex> EventLogger.hello()
      :world

  """

  use Application
  require Logger
  require Utilities.Logging

  require DatabaseEngine.Utils.EventLogger

  def multiple_event() do
    event = %{
      "entity_name" => "entity_name",
      "entity_id" => "entity_id",
      "state" => "state",
      "additional_data" => %{
        "__index" => %{
          "s1" => "s1",
          "s2" => "s2",
          "i1" => 1,
          "i2" => 2
        }
      }
    }

    DatabaseEngine.Utils.EventLogger.log_events([event, event, event, event, event])
  end

  def simple_log() do
    :ok =
      DatabaseEngine.Utils.EventLogger.log_event("test_entity", "test_id", "entity_state", %{
        "__index" => %{
          "s1" => "hello",
          "s2" => "koo",
          "i1" => 12,
          "d1" => 50.1
        }
      })

    :ok
  end

  @event_log_topic_name "log_event"
  def start(_type, _args) do
    DatabaseEngine.DurableQueue.start_consumer_group(
      @event_log_topic_name,
      @event_log_topic_name,
      EventLogger.Consumer.DruidPublisher
    )
  end
end
