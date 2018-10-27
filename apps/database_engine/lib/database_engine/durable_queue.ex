defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging

  alias Utilities.Serializers.JSONSerializer, as: Serializer
  alias Utilities.Logging

  def serialize(obj) do
    Logging.debug(fn -> "Called. with obj: #{obj}" end)
    rtn = Serializer.serialize(obj)

    case rtn do
      {:ok, serialized} ->
        Logging.debug(fn ->
          "Serialized Successfully, Serialized Result: #{serialized}"
        end)

      _ ->
        Logging.debug(fn -> "Serializer Retuned unexpected result: #{rtn}. Return :nok" end)
        :nok
    end
  end

  def deserialize(string) do
    Logging.debug(fn -> "Called with parameters: #{string}" end)

    case Serializer.deserialize(string) do
      {:ok, obj} ->
        Logging.debug(fn -> "Deserialized Successfully, Returns: #{obj}" end)
        obj

      V ->
        Logging.debug(fn -> "Deserializer Faild, Returned: #{v}" end)
        nil
    end
  end

  def enqueue(topic_name, partition_number, object) do
    Logging.debug(fn ->
      "Called with parameters: topic_name: #{topic_name} , partition_number: #{partition_number} object: #{
        object
      }"
    end)

    case serialize(object) do
      :nok ->
        Logging.debug("Could not Serialize object to iodata, Return :nok")
        :nok

      serialized ->
        rtn = KafkaEx.produce(topic_name, partition_number, serialized)
        Logging.debug(fn -> "Return Value: #{rtn}" end)
        rtn
    end
  end

  def enqueue(topic_name, object) do
    Logging.debug(fn -> "Called with parameters: topic_name: #{topic_name} object: #{object}" end)

    #    @fixme: increase round-robin process using process dictionary, increamenting by +1
    Logging.debug(fn -> "calculating kafka topic partitions ..." end)

    partitions =
      KafkaEx.metadata()
      |> Map.get(:topic_metadatas)
      |> Enum.filter(fn x -> x.topic == topic_name end)
      |> Enum.flat_map(fn k -> k.partition_metadatas end)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    case partitions do
      l when is_list(l) ->
        Logging.debug(fn -> "calulated partitions for topic: #{topic_name} are: #{l}" end)
        enqueue(topic_name, hd(l), object)

      [] ->
        Logging.debug(fn ->
          "no partition found for topic: #{topic_name}, may be there is no topic "
        end)

        :ok
    end
  end

  def start_consumer_group(
        topic_name,
        consumer_group_name,
        consumer_module \\ DatabaseEngine.DurableQueue.Consumers.SimpleLogConsumer
      ) do
    Logging.debug(fn ->
      """
      Called With parameters: 
      topic_name: #{topic_name} 
      consumer_group_name: #{consumer_group_name} 
      consumer_module: #{consumer_module}
      """
    end)

    child_spec = %{
      :id => topic_name,
      :start =>
        {KafkaEx.ConsumerGroup, :start_link,
         [consumer_module, consumer_group_name, [topic_name], []]},
      :restart => :permanent,
      :type => :worker
    }

    Logging.debug(fn -> "Call supervisor start child with params: #{child_spec}" end)

    r =
      DynamicSupervisor.start_child(
        DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor,
        child_spec
      )

    Logging.debug(fn -> "Returns: #{r}" end)
    r
  end
end
