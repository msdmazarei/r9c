defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging

  alias Utilities.Serializers.JSONSerializer, as: Serializer
  alias Utilities.Logging

  @spec serialize(any()) :: String.t() | :nok
  def serialize(obj) do
    Logging.debug("Called. with obj:~p", [obj])
    rtn = Serializer.serialize(obj)
    case rtn do
      {:ok, serialized} ->
        Logging.debug("Serialized Successfully, Serialized Result:~p", [serialized])
        serialized
      _ ->
        Logging.debug("Serializer Retuned unexpected result:~p. Return :nok", [rtn])
        :nok
    end
  end

  @spec deserialize(String.t()) :: term | :nil
  def deserialize(string) do
    Logging.debug("Called with parameters:~p", [string])
    case Serializer.deserialize(string) do
      {:ok, obj} ->
        Logging.debug("Deserialized Successfully, Returns:~p", [obj])
        obj
      V ->
        Logging.debug("Deserializer Faild, Returned:~p", [V])
        :nil
    end
  end

  @spec enqueue(String.t(), integer(), term) :: :nok | :ok
  def enqueue(topic_name, partition_number, object) do
    Logging.debug(
      "Called with parameters: topic_name:~p , partition_number:~p object:~p",
      [topic_name, partition_number, object]
    )
    case serialize(object) do
      :nok ->
        Logging.debug("Could not Serialize object to iodata, Return :nok")
        :nok
      serialized ->
        rtn = KafkaEx.produce(topic_name, partition_number, serialized)
        Logging.debug("Return Value: ~p", [rtn])
        rtn


    end

  end

  @spec enqueue(String.t(), term) :: :ok | :nok
  def enqueue(topic_name, object) do
    Logging.debug("Called with parameters: topic_name:~p object:~p", [topic_name, object])

    #    @fixme: increase round-robin process using process dictionary, increamenting by +1
    Logging.debug("calculating kafka topic partitions ...")
    partitions = (KafkaEx.metadata()
                  |> Map.get(:topic_metadatas)
                  |> Enum.filter(fn x -> x.topic == topic_name end)
                  |> Enum.flat_map(fn k -> k.partition_metadatas end)
                  |> Enum.map(fn x -> x.partition_id end)
                  |> Enum.shuffle())

    case  partitions do
      l when is_list(l) ->
        Logging.debug("calulated partitions for topic:~p are:~p", [topic_name, l])
        enqueue(topic_name, hd(l), object)
      [] ->
        Logging.debug("no partition found for topic:~p, may be there is no topic ", [topic_name])
        :ok


    end
  end


  @spec start_consumer_group(String.t(), String.t(), module()) :: {:ok, pid()}
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
    Logging.debug(
      "Called With parameters: topic_name:~p consumer_group_name:~p consumer_module:~p",
      [topic_name, consumer_group_name, consumer_module]
    )
    child_spec = %{
      :id => topic_name,
      :start => {KafkaEx.ConsumerGroup, :start_link, [consumer_module, consumer_group_name, [topic_name], []]},
      :restart => :permanent,
      :type => :worker
    }
    Logging.debug("Call supervisor start child with params:~p", [child_spec])
    r = DynamicSupervisor.start_child(DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor, child_spec)
    Logging.debug("Returns:~p", [r])
    r
  end


end


