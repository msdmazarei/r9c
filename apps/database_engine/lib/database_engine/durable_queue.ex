defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging

  alias Utilities.Serializers.JSONSerializer, as: Serializer
  alias Utilities.Logging

  def serialize(obj) do
    Logging.debug("Called. with obj: #{obj}")
    rtn = Serializer.serialize(obj)

    case rtn do
      {:ok, serialized} ->
        Logging.debug("Serialized Successfully, Serialized Result: #{serialized}")

      _ ->
        Logging.debug("Serializer Retuned unexpected result: #{rtn}. Return :nok")
        :nok
    end
  end

  def deserialize(string) do
    Logging.debug("Called with parameters: #{string}")

    case Serializer.deserialize(string) do
      {:ok, obj} ->
        Logging.debug("Deserialized Successfully, Returns: #{obj}")
        obj

      v ->
        Logging.debug("Deserializer Faild, Returned: #{v}")
        nil
    end
  end

  def enqueue(topic_name, partition_number, object) do
    Logging.debug("""
    Called with parameters: 
      topic_name: #{topic_name}
      partition_number: #{partition_number}
      object: #{object}
    """)

    case serialize(object) do
      :nok ->
        Logging.debug("Could not Serialize object to iodata, Return :nok")
        :nok

      serialized ->
        rtn = KafkaEx.produce(topic_name, partition_number, serialized)
        Logging.debug("Return Value: #{rtn}")
        rtn
    end
  end

  def enqueue(topic_name, object) do
    Logging.debug("Called with parameters: topic_name: #{topic_name} object: #{object}")

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
        Logging.debug("calulated partitions for topic: #{topic_name} are: #{l}")
        enqueue(topic_name, hd(l), object)

      [] ->
        Logging.debug("no partition found for topic: #{topic_name}, may be there is no topic ")

        :ok
    end
  end

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

    Logging.debug("Call supervisor start child with params: #{child_spec}")

    r =
      DynamicSupervisor.start_child(
        DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor,
        child_spec
      )

    Logging.debug("Returns: #{r}")
    r
  end
end
