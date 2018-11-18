defprotocol DatabaseEngine.DurableQueue.Deserialize do
  def deserialize(data)
end

defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging

  alias Utilities.Serializers.JSONSerializer, as: Serializer
  alias Utilities.Logging

  @spec serialize(any()) :: String.t() | :nok
  def serialize(obj) do
    Logging.debug("Called. with obj: ~p", [obj])
    rtn = Serializer.serialize(obj)

    case rtn do
      {:ok, serialized} ->
        Logging.debug("Serialized Successfully, Serialized Result: ~p", [serialized])
        serialized

      _ ->
        Logging.debug("Serializer Retuned unexpected result: #{rtn}. Return :nok")
        :nok
    end
  end

  @spec deserialize(String.t()) :: term | nil
  def deserialize(string) do
    Logging.debug("Called with parameters: #{string}")

    case Serializer.deserialize(string) do
      {:ok, obj} ->
        Logging.debug("Deserialized Successfully, Returns: ~p", [obj])

        case obj["__orig_struct__"] do
          nil ->
            obj

          _ ->
            DatabaseEngine.DurableQueue.Deserialize.deserialize(obj)
        end

      v ->
        Logging.debug("Deserializer Faild, Returned: ~p", [v])
        nil
    end
  end

  @spec enqueue(String.t(), integer(), any) :: :nok | :ok
  def enqueue(topic_name, partition_number, object) do
    Logging.debug(
      """
      Called with parameters:
        topic_name: #{topic_name}
        partition_number: ~p
        object: ~p
      """,
      [partition_number, object]
    )

    case serialize(object) do
      :nok ->
        Logging.debug("Could not Serialize object to iodata, Return :nok")
        :nok

      serialized ->
        Logging.debug(
          "Calling Kafka Produce topic_name:#{topic_name} partition_number:#{partition_number} serialized:#{
            serialized
          }"
        )

        rtn = KafkaEx.produce(topic_name, partition_number, serialized)
        Logging.debug("Return Value: #{rtn}")
        rtn
    end
  end

  @spec enqueue(String.t(), any) :: :ok | :nok
  def enqueue(topic_name, object) do
    Logging.debug("Called with parameters: topic_name: #{topic_name} object: ~p", [object])

    #    @fixme: increase round-robin process using process dictionary, increamenting by +1
    Logging.debug("calculating kafka topic partitions ...")

    partitions =
      KafkaEx.metadata()
      |> Map.get(:topic_metadatas)
      |> Enum.filter(fn x -> x.topic == topic_name end)
      |> Enum.flat_map(fn k -> k.partition_metadatas end)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    case partitions do
      l when is_list(l) ->
        Logging.debug("calulated partitions for topic: #{topic_name} are: ~p", [l])
        enqueue(topic_name, hd(l), object)

      [] ->
        Logging.debug("no partition found for topic: #{topic_name}, may be there is no topic ")

        :ok
    end
  end

  @spec start_consumer_group(String.t(), String.t(), module()) ::
          {:ok, pid()}
          | {:ok, pid(), info :: term()}
          | :ignore
          | {
              :error,
              {:already_started, pid()} | :max_children | term()
            }
  def start_consumer_group(
        topic_name,
        consumer_group_name,
        consumer_module \\ DatabaseEngine.DurableQueue.Consumers.SimpleLogConsumer
      ) do
    Logging.debug("""
    Called With parameters:
    topic_name: #{topic_name}
    consumer_group_name: #{consumer_group_name}
    consumer_module: #{consumer_module}
    """)

    child_spec = %{
      :id => topic_name,
      :start =>
        {KafkaEx.ConsumerGroup, :start_link,
         [consumer_module, consumer_group_name, [topic_name], []]},
      :restart => :permanent,
      :type => :worker
    }

    Logging.debug("Call supervisor start child with params: ~p", [child_spec])

    r =
      DynamicSupervisor.start_child(
        DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor,
        child_spec
      )

    Logging.debug("Returns: ~p", [r])
    r
  end

  def stop_consumer_group(pid) do
    DynamicSupervisor.terminate_child(
      DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor,
      pid
    )
  end
end
