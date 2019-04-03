defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require Utilities.Serializers.BinSerializer
  alias Utilities.Serializers.BinSerializer, as: Serializer
  alias Utilities.Logging

  @spec serialize(any()) :: String.t() | :nok
  def serialize(obj) do
    Logging.debug("Called.", [])
    Serializer.serialize(obj)

    # rtn = Utilities.Serializers.JSONSerializer.serialize(obj)
    # Logging.debug("serialized. r:~p", [rtn])

    # case rtn do
    #   {:ok, serialized} ->
    #     Logging.debug("Serialized Successfully, Serialized Result: ~p", [serialized])
    #     serialized

    #   _ ->
    #     Logging.warn("Serializer Retuned unexpected result: #{rtn}. Return :nok")
    #     :nok
    # end
  end

  @spec deserialize(String.t()) :: term | nil
  def deserialize(string) do
    Logging.debug("Called with parameters: #{string}")

    try do
      Serializer.deserialize(string)
    rescue
      v ->
        Logging.debug("Deserializer Faild, Returned: ~p", [v])

        nil
    end

    # case Serializer.deserialize(string) do
    #   {:ok, obj} ->
    #     Logging.debug("Deserialized Successfully, Returns: ~p", [obj])

    #     case obj["__orig_struct__"] do
    #       nil ->
    #         obj

    #       _ ->
    #         DatabaseEngine.DurableQueue.Deserialize.deserialize(obj)
    #     end

    #   v ->
    #     Logging.debug("Deserializer Faild, Returned: ~p", [v])
    #     nil
    # end
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

  def get_partitions_from_kafka(topic_name) do
    KafkaEx.metadata()
    |> Map.get(:topic_metadatas)
    |> Enum.filter(fn x -> x.topic == topic_name end)
    |> Enum.flat_map(fn k -> k.partition_metadatas end)
  end

  def get_partitions_of_topic(topic_name) do
    key = "durable_queue_partitions_of_" <> topic_name

    from_kafka = fn ->
      p = get_partitions_from_kafka(topic_name)
      v = {Utilities.now(), p}
      DatabaseEngine.Interface.KV.set(key, v)

      if p == nil do
        []
      else
        p
      end
    end

    partitions =
      case DatabaseEngine.Interface.KV.get(key) do
        nil ->
          from_kafka.()

        {time, partiotions} ->
          if time + 300_000 > Utilities.now() do
            partiotions
          else
            from_kafka.()
          end
      end
  end

  @spec enqueue(String.t(), any) :: :ok | :nok
  def enqueue(topic_name, object) do
    Logging.debug("Called with parameters: topic_name:~p object: ~p", [topic_name, object])

    #    @fixme: increase round-robin process using process dictionary, increamenting by +1
    Logging.debug("calculating kafka topic partitions ...")

    partitions =
      get_partitions_of_topic(topic_name)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    case partitions do
      l when is_list(l) and length(l) > 0 ->
        Logging.debug("calulated partitions for topic: ~p are: ~p", [topic_name, l])
        enqueue(topic_name, hd(l), object)

      [] ->
        Logging.warn(
          "no partition found for topic: ~p, may be there is no topic. data to enqueue: ~p",
          [topic_name, object]
        )

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

  def get_consumers_pid() do
    list_of_sups =
      DynamicSupervisor.which_children(
        DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor
      )
      |> Enum.map(fn {_, x, _, _} -> x end)

    sup_states = list_of_sups |> Enum.map(&:sys.get_state/1)

    consumer_sup_pids =
      sup_states
      |> Enum.map(fn x ->
        {_, _, _, {_, %{:consumer => cons}}, _, _, _, _, _, _, _} = x
        cons |> elem(1)
      end)

    consumer_sup_pids
    |> Enum.map(fn x ->
      {_, _, _, _, {_, cosumers_pid_map}, _, _, _, _, _, _} = :sys.get_state(x)

      cosumers_pid_map
      |> Map.to_list()
      |> Enum.map(fn {k, v} -> {v |> Enum.at(2), v |> Enum.at(3), k} end)
      |> Enum.reduce({nil, %{}}, fn x, acc ->
        {na, k, v} = x

        {_, ma} = acc
        ma = ma |> Map.put(k, v)

        acc = {na, ma}
        acc
      end)
    end)
  end

  def get_consumers_stats() do
    consumers = get_consumers_pid()

    consumers
    |> Enum.map(fn {qname, parts_map} ->
      parts_stat =
        parts_map
        |> Map.to_list()
        |> Enum.map(fn {part_no, part_pid} ->
          stat =
            case :sys.get_state(part_pid) do
              %KafkaEx.GenConsumer.State{consumer_state: m} when is_map(m) ->
                %{
                  "arrived" => m["arrived_messages"],
                  "processed" => m["processed_messages"],
                  "process_duration" => m["process_duration"]
                }

              _ ->
                %{"arrived" => 0, "processed" => 0, "process_duration" => 0}
            end

          {part_no, stat}
        end)
        |> Map.new()

      {ta, tp, td} =
        parts_stat
        |> Map.to_list()
        |> Enum.reduce({0, 0, 0}, fn {_, m}, {t1, t2, t3} ->
          t1 = t1 + (m["arrived"]||0)
          t2 = t2 + (m["processed"]||0)
          t3 = t3 + (m["process_duration"] || 0)
          {t1, t2, t3}
        end)

      parts_stat =
        parts_stat
        |> Map.put("total", %{"arrived" => ta, "processed" => tp, "process_duration" => td})

      {qname, parts_stat}
    end)
  end
end

defprotocol DatabaseEngine.DurableQueue.Deserialize do
  def deserialize(data)
end
