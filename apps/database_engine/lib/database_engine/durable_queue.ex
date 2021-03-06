defmodule DatabaseEngine.DurableQueue do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require Utilities.Serializers.BinSerializer
  alias Utilities.Serializers.BinSerializer, as: Serializer
  alias Utilities.Logging

  @kafka_max_message_size_to_produce 1024 * 1024 - 100

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

  @spec enqueue_string(binary(), integer(), binary()) ::
          :leader_not_available
          | nil
          | :ok
          | binary()
          | maybe_improper_list(
              binary() | maybe_improper_list(any(), binary() | []) | byte(),
              binary() | []
            )
          | {:error, any()}
          | {:ok, integer()}
  def enqueue_string(topic_name, partition_number, str) do
    Logging.debug("called. topicname:~p pn:~p str:~p", [topic_name, partition_number, str])
    rtn = KafkaEx.produce(topic_name, partition_number, str)
    Logging.debug("Return Value: #{rtn}")
    rtn
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

  @spec enqueue_string(String.t(), String.t()) :: :nok | :ok
  def enqueue_string(topic_name, str) do
    Logging.debug("Called with parameters: topic_name:~p string: ~p", [topic_name, str])
    Logging.debug("calculating kafka topic partitions ...")

    partitions =
      get_partitions_of_topic(topic_name)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    case partitions do
      l when is_list(l) and length(l) > 0 ->
        Logging.debug("calulated partitions for topic: ~p are: ~p", [topic_name, l])

        case enqueue_string(topic_name, hd(l), str) do
          :ok ->
            :ok

          {:ok, _} ->
            :ok

          e ->
            Logging.warn("not proper result from kafkaex. result is:~p", [e])
            :nok
        end

      [] ->
        Logging.warn(
          "no partition found for topic: ~p, may be there is no topic. data to enqueue: ~p",
          [topic_name, str]
        )

        :nok
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

  @spec enqueue_string_list(String.t(), [String.t()]) ::
          {[{integer(), :ok | :nok}], %{optional(<<_::40, _::_*24>>) => any()}}
  def enqueue_string_list(topic_name, list_of_strings) do
    #sample return value
    #     {[{0, :ok}, {1, :ok}, {2, :ok}, {3, :ok}],
    #  %{
    #    "grouping" => 0,
    #    "partitions_durations" => [
    #      {0, %{"agg_binaries" => 0, "kafka_push" => 6, "serialization" => 0}},
    #      {1, %{"agg_binaries" => 0, "kafka_push" => 8, "serialization" => 0}},
    #      {2, %{"agg_binaries" => 0, "kafka_push" => 15, "serialization" => 0}},
    #      {3, %{"agg_binaries" => 0, "kafka_push" => 18, "serialization" => 0}}
    #    ],
    #    "total" => %{
    #      "agg_binaries" => 0,
    #      "kafka_push" => 18,
    #      "serialization" => 0,
    #      "total" => 18
    #    }
    #  }}

    partitions =
      get_partitions_of_topic(topic_name)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    durations = %{}

    case partitions do
      l when is_list(l) and length(l) > 0 ->
        len_parts = length(l)

        st_time = Utilities.now()

        partition_for_items =
          list_of_strings
          |> Enum.with_index()
          |> Enum.map(fn {_, i} ->
            rem(i, len_parts)
          end)

        list_of_strings_with_index = list_of_strings |> Enum.with_index()

        part_obj = Enum.zip(partition_for_items, list_of_strings_with_index)
        map_part_obj = part_obj |> Enum.group_by(fn {p, _} -> p end, fn {_, o} -> o end)

        du_time = Utilities.now() - st_time

        durations = durations |> Map.put("grouping", du_time)

        tasks =
          map_part_obj
          |> Map.to_list()
          |> Enum.map(fn {p, objs_list_with_index} ->
            Task.async(fn ->
              sub_duration = %{}

              st_time = Utilities.now()

              serialized_obj_list_with_index = objs_list_with_index
              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("serialization", du_time)

              st_time = Utilities.now()

              list_of_list_of_bins_with_index =
                Utilities.agg_binaries_till_reach_to_size(
                  serialized_obj_list_with_index,
                  fn {b, _} -> byte_size(b) end,
                  @kafka_max_message_size_to_produce
                )

              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("agg_binaries", du_time)

              result_of_sending_msg =
                list_of_list_of_bins_with_index
                |> Enum.map(fn list_of_bin_with_index ->
                  list_of_kafka_msgs =
                    list_of_bin_with_index
                    |> Enum.map(fn {x_bin, _} ->
                      %KafkaEx.Protocol.Produce.Message{value: x_bin}
                    end)

                  r =
                    KafkaEx.produce(%KafkaEx.Protocol.Produce.Request{
                      topic: topic_name,
                      partition: p,
                      required_acks: 1,
                      messages: list_of_kafka_msgs
                    })

                  case r do
                    :ok ->
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :ok} end)

                    {:ok, _} ->
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :ok} end)

                    other ->
                      Logging.error("unexpected result for enqueue:~p", [other])
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :nok} end)
                  end
                end)

              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("kafka_push", du_time)
              rtn_list = result_of_sending_msg |> List.flatten()
              {rtn_list, {p, sub_duration}}
            end)
          end)

        results = tasks |> Enum.map(fn t -> Task.await(t) end)
        result_lists = results |> Enum.map(fn {l, _} -> l end)
        result_part_durations = results |> Enum.map(fn {_, d} -> d end)

        durations = durations |> Map.put("partitions_durations", result_part_durations)

        max_sub_duration =
          result_part_durations
          |> Enum.map(fn {_, du} ->
            sum_all_values = du |> Map.to_list() |> Enum.map(fn {_, v} -> v end) |> Enum.sum()
            du |> Map.put("total", sum_all_values)
          end)
          |> Enum.max_by(fn x -> x["total"] end)

        durations = durations |> Map.put("total", max_sub_duration)

        rtn = result_lists |> List.flatten()
        {rtn, durations}

      _ ->
        rtn = list_of_strings |> Enum.with_index() |> Enum.map(fn _, i -> {i, :nok} end)
        {rtn, durations}
    end
  end

  def enqueue_list(topic_name, list_of_objects) do
    partitions =
      get_partitions_of_topic(topic_name)
      |> Enum.map(fn x -> x.partition_id end)
      |> Enum.shuffle()

    durations = %{}

    case partitions do
      l when is_list(l) and length(l) > 0 ->
        len_parts = length(l)

        st_time = Utilities.now()

        partition_for_items =
          list_of_objects
          |> Enum.with_index()
          |> Enum.map(fn {_, i} ->
            rem(i, len_parts)
          end)

        list_of_objects_with_index = list_of_objects |> Enum.with_index()

        part_obj = Enum.zip(partition_for_items, list_of_objects_with_index)
        map_part_obj = part_obj |> Enum.group_by(fn {p, _} -> p end, fn {_, o} -> o end)

        du_time = Utilities.now() - st_time

        durations = durations |> Map.put("grouping", du_time)

        tasks =
          map_part_obj
          |> Map.to_list()
          |> Enum.map(fn {p, objs_list_with_index} ->
            Task.async(fn ->
              sub_duration = %{}

              st_time = Utilities.now()

              serialized_obj_list_with_index =
                objs_list_with_index |> Enum.map(fn {obj, indx} -> {serialize(obj), indx} end)

              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("serialization", du_time)

              st_time = Utilities.now()

              list_of_list_of_bins_with_index =
                Utilities.agg_binaries_till_reach_to_size(
                  serialized_obj_list_with_index,
                  fn {b, _} -> byte_size(b) end,
                  @kafka_max_message_size_to_produce
                )

              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("agg_binaries", du_time)

              result_of_sending_msg =
                list_of_list_of_bins_with_index
                |> Enum.map(fn list_of_bin_with_index ->
                  list_of_kafka_msgs =
                    list_of_bin_with_index
                    |> Enum.map(fn {x_bin, _} ->
                      %KafkaEx.Protocol.Produce.Message{value: x_bin}
                    end)

                  r =
                    KafkaEx.produce(%KafkaEx.Protocol.Produce.Request{
                      topic: topic_name,
                      partition: p,
                      required_acks: 1,
                      messages: list_of_kafka_msgs
                    })

                  case r do
                    :ok ->
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :ok} end)

                    {:ok, _} ->
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :ok} end)

                    other ->
                      Logging.error("unexpected result for enqueue:~p", [other])
                      list_of_bin_with_index |> Enum.map(fn {_, i} -> {i, :nok} end)
                  end
                end)

              du_time = Utilities.now() - st_time
              sub_duration = sub_duration |> Map.put("kafka_push", du_time)
              rtn_list = result_of_sending_msg |> List.flatten()
              {rtn_list, {p, sub_duration}}
            end)
          end)

        results = tasks |> Enum.map(fn t -> Task.await(t) end)
        result_lists = results |> Enum.map(fn {l, _} -> l end)
        result_part_durations = results |> Enum.map(fn {_, d} -> d end)

        durations = durations |> Map.put("partitions_durations", result_part_durations)

        max_sub_duration =
          result_part_durations
          |> Enum.map(fn {_, du} ->
            sum_all_values = du |> Map.to_list() |> Enum.map(fn {_, v} -> v end) |> Enum.sum()
            du |> Map.put("total", sum_all_values)
          end)
          |> Enum.max_by(fn x -> x["total"] end)

        durations = durations |> Map.put("total", max_sub_duration)

        rtn = result_lists |> List.flatten()
        {rtn, durations}

      _ ->
        rtn = list_of_objects |> Enum.with_index() |> Enum.map(fn _, i -> {i, :nok} end)
        {rtn, durations}
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
    Logging.debug(
      "Called With parameters: topic_name: ~p consumer_group_name: ~p consumer_module: ~p",
      [
        topic_name,
        consumer_group_name,
        consumer_module
      ]
    )

    child_spec = %{
      :id => topic_name,
      :start =>
        {KafkaEx.ConsumerGroup, :start_link,
         [
           consumer_module,
           consumer_group_name,
           [topic_name],
           [
             # setting for the ConsumerGroup
             heartbeat_interval: 5_000,
             # this setting will be forwarded to the GenConsumer
             commit_interval: 5_000,
             commit_threshold: 5_000,
             fetch_options: [
               max_bytes: 3_000_000
             ]
           ]
         ]},
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

  def stop_consumer_greoup_by_workder_pid(pid) do
    Logging.debug("Called. Pid:~p", [pid])
    p_info = :erlang.process_info(pid)
    pid_sup_consumer_group = p_info[:dictionary][:"$ancestors"] |> Enum.at(1)
    Logging.debug("pid_sup_consumer_group:~p", [pid_sup_consumer_group])
    stop_consumer_group(pid_sup_consumer_group)
  end

  @spec stop_consumer_group(pid()) :: :ok | {:error, :not_found}
  def stop_consumer_group(pid) do
    DynamicSupervisor.terminate_child(
      DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor,
      pid
    )
  end

  def stop_consumer_group() do
    DynamicSupervisor.stop(DatabaseEngine.DurableQueue.ConsumerGroupWorkers.Supervisor)
  end

  def get_running_consumers() do
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
      |> Enum.map(fn {consumer_pid,
                      [consumer_module, consumer_group, q_name, partition_no, consumer_config]} ->
        consumer_config =
          if consumer_config[:fetch_options] do
            d = consumer_config |> Map.new()
            d |> Map.put(:fetch_options, d[:fetch_options] |> Map.new())
          else
            consumer_config |> Map.new()
          end

        {consumer_group,
         %{
           "pid" => consumer_pid,
           "q_name" => q_name,
           "partition_no" => partition_no,
           "config" => consumer_config,
           "consumer_module" => consumer_module
         }}
      end)
      |> Enum.group_by(fn {i, _} -> i end, fn {_, v} -> v end)
    end)
    |> Enum.reduce(%{}, fn i, acc ->
      acc |> Map.merge(i)
    end)
  end

  def get_running_consumer_stats() do
    r = get_running_consumers()

    r
    |> Map.to_list()
    |> Enum.map(fn {cn, pl} ->
      {cn,
       pl
       |> Enum.map(fn x ->
         worker_pid = x |> Map.get("pid")
         partiotion_no = x |> Map.get("partition_no")

         if worker_pid do
           %{
             partiotion_no =>
               DatabaseEngine.Interface.LProcessData.get({"consumer", worker_pid}) || %{}
           }
         else
           %{partiotion_no => %{}}
         end
       end)
       |> Enum.reduce(%{}, fn x, acc ->
         Map.merge(x, acc)
       end)}
    end)
    |> Map.new()
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
            case DatabaseEngine.Interface.LProcessData.get({"consumer", part_pid}) do
              m when is_map(m) ->
                %{
                  "arrived" => m["arrived_messages"],
                  "processed" => m["processed_messages"],
                  "process_duration" => m["process_duration"],
                  "dropped_messages_cause_of_retry" => m["dropped_messages_cause_of_retry"],
                  "need_to_requeue_count" => m["need_to_requeue_count"],
                  "process_creation_time" => m["process_creation_time"],
                  "send_message_to_process_time" => m["send_message_to_process_time"],
                  "successfully_delivered_to_uprocess" => m["successfully_delivered_to_uprocess"]
                }

              _ ->
                %{
                  "arrived" => 0,
                  "processed" => 0,
                  "process_duration" => 0,
                  "dropped_messages_cause_of_retry" => 0,
                  "need_to_requeue_count" => 0,
                  "process_creation_time" => 0,
                  "process_duration" => 0,
                  "send_message_to_process_time" => 0,
                  "successfully_delivered_to_uprocess" => 0
                }
            end

          {part_no, stat}
        end)
        |> Map.new()

      total_res =
        parts_stat
        |> Map.to_list()
        |> Enum.reduce(%{}, fn {_, m}, res ->
          r =
            m
            |> Map.to_list()
            |> Enum.reduce(res, fn {k, v}, acc ->
              acc |> Map.put(k, (acc[k] || 0) + (v || 0))
            end)

          r
        end)

      parts_stat =
        parts_stat
        |> Map.put("total", total_res)

      {qname, parts_stat}
    end)
  end

  def get_consumers_states() do
    consumers = get_consumers_pid()

    consumers
    |> Enum.map(fn {qname, parts_map} ->
      parts_stat =
        parts_map
        |> Map.to_list()
        |> Enum.map(fn {part_no, part_pid} ->
          state =
            case :sys.get_state(part_pid) do
              %KafkaEx.GenConsumer.State{consumer_state: m} ->
                m
            end
        end)

      {qname, parts_stat}
    end)
  end

  def stop_kafka_ex() do
    Logging.debug("called.")
    Application.stop(:kafka_ex)
  end

  def start_kafka_ex() do
    Logging.debug("called.")
    all_brokers = DatabaseEngine.Interface.SystemConfig.KafkaBroker.Repo.get_all()
    all_brokers_hosts = all_brokers |> Enum.map(fn x -> x.host end)
    brokers = all_brokers_hosts |> Enum.join(",")
    Logging.debug("brokers:~p", [brokers])
    Application.put_env(:kafka_ex, :brokers, brokers)
    r = Application.start(:kafka_ex)
    Logging.debug("started result:~p", [r])
    r
  end
end

defprotocol DatabaseEngine.DurableQueue.Deserialize do
  def deserialize(data)
end
