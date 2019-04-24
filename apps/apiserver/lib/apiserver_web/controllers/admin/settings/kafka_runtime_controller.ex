defmodule ApiserverWeb.Admin.Settings.Kafka.Runtime.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities

  @api_duration "api_duration"
  def params_to_struct(params) do
    Utilities.to_struct(Model, params)
  end

  def response_body(st_time, result) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => result
    }

    rtn
  end

  def start_kafka(conn, params) do
    Logging.debug("called.")
    st_time = Utilities.now()
    nodes_to_start_kafka = (params["nodes"] || []) |> Enum.map(fn x -> String.to_atom(x) end)
    all_active_nodes = Utilities.all_active_nodes()

    down_nodes =
      nodes_to_start_kafka |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) == false end)

    active_nodes_to_start =
      nodes_to_start_kafka |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) end)

    async_tasks_result =
      active_nodes_to_start
      |> Enum.map(fn x ->
        Utilities.remote_async_task(x, DatabaseEngine.DurableQueue, :start_kafka_ex, [])
      end)

    remote_results = Utilities.await_multi_task(async_tasks_result, 5000, :no_reponse)
    node_result = Enum.zip(active_nodes_to_start, remote_results)

    rtn =
      down_nodes
      |> Enum.reduce(node_result, fn nodename, res ->
        [{nodename, :nodedown} | res]
      end)
      |> Enum.map(fn {n, res} ->
        res =
          case res do
            :ok -> :ok
            {:badrpc, r} -> Utilities.to_string(r)
            {:error, {:already_started, _}} -> "already_started"
            v -> Utilities.to_string(v)
          end

        {n, res}
      end)
      |> Map.new()

    rtn = response_body(st_time, rtn)
    Logging.info("kafka application started on nodes:~p", [nodes_to_start_kafka])

    conn |> send_response(200, rtn)
  end

  def stop_kafka(conn, params) do
    st_time = Utilities.now()
    nodes_to_stop_kafka = (params["nodes"] || []) |> Enum.map(fn x -> String.to_atom(x) end)
    all_active_nodes = Utilities.all_active_nodes()

    down_nodes =
      nodes_to_stop_kafka |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) == false end)

    active_nodes_to_stop =
      nodes_to_stop_kafka |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) end)

    async_tasks_result =
      active_nodes_to_stop
      |> Enum.map(fn x ->
        Utilities.remote_async_task(x, DatabaseEngine.DurableQueue, :stop_kafka_ex, [])
      end)

    remote_results = Utilities.await_multi_task(async_tasks_result, 5000, :no_reponse)
    node_result = Enum.zip(active_nodes_to_stop, remote_results)

    rtn =
      down_nodes
      |> Enum.reduce(node_result, fn nodename, res ->
        [{nodename, :nodedown} | res]
      end)
      |> Enum.map(fn {n, res} ->
        res =
          case res do
            :ok -> :ok
            {:badrpc, r} -> Utilities.to_string(r)
            {:error, {:not_started, _}} -> "not_started"
            v -> Utilities.to_string(v)
          end

        {n, res}
      end)
      |> Map.new()

    rtn = response_body(st_time, rtn)
    conn |> send_response(200, rtn)
  end

  def get_brokers(conn, params) do
    Logging.debug("called")
    st_time = Utilities.now()
    nodes = params["nodes"] || []
    nodes = nodes |> Enum.map(&String.to_atom/1)
    all_active_nodes = Utilities.all_active_nodes()
    active_nodes = nodes |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) end)
    down_nodes = nodes -- active_nodes

    tasks =
      active_nodes
      |> Enum.map(fn x ->
        Utilities.remote_async_task(x, KafkaEx, :metadata, [])
      end)

    resps = Utilities.await_multi_task(tasks, 5000, :noresponse)

    resps =
      resps
      |> Enum.map(fn x ->
        case x do
          %KafkaEx.Protocol.Metadata.Response{brokers: b} -> b |> Enum.map(&Map.from_struct/1)
          _ -> x
        end
      end)

    node_resp = Enum.zip(active_nodes, resps)
    node_resp = (node_resp ++ (down_nodes |> Enum.map(fn x -> {x, :nodedown} end))) |> Map.new()
    rtn = response_body(st_time, node_resp)
    conn |> send_response(200, rtn)
  end

  def get_topics(conn, params) do
    Logging.debug("called")
    st_time = Utilities.now()
    nodes = params["nodes"] || []
    nodes = nodes |> Enum.map(&String.to_atom/1)
    all_active_nodes = Utilities.all_active_nodes()
    active_nodes = nodes |> Enum.filter(fn x -> Enum.member?(all_active_nodes, x) end)
    down_nodes = nodes -- active_nodes

    tasks =
      active_nodes
      |> Enum.map(fn x ->
        Utilities.remote_async_task(x, KafkaEx, :metadata, [])
      end)

    resps = Utilities.await_multi_task(tasks, 5000, :noresponse)

    resps =
      resps
      |> Enum.map(fn x ->
        case x do
          %KafkaEx.Protocol.Metadata.Response{topic_metadatas: b} ->
            b
            |> Enum.map(fn t ->
              partitions = t.partition_metadatas |> Enum.map(&Map.from_struct/1)
              t |> Map.from_struct() |> Map.put(:partition_metadatas, partitions)
            end)

          _ ->
            x
        end
      end)

    node_resp = Enum.zip(active_nodes, resps)
    node_resp = (node_resp ++ (down_nodes |> Enum.map(fn x -> {x, :nodedown} end))) |> Map.new()
    rtn = response_body(st_time, node_resp)
    conn |> send_response(200, rtn)
  end

  def delete_topic(conn, params) do
    Logging.debug("Called. params:~p", [params])
    st_time = Utilities.now()
    topic_to_delete = params["topic"] || ""

    del_result = KafkaEx.delete_topics([topic_to_delete])
    Logging.debug("delete result:~p", [del_result])

    {status, json_response} =
      case del_result do
        %KafkaEx.Protocol.DeleteTopics.Response{
          topic_errors: [
            %KafkaEx.Protocol.DeleteTopics.TopicError{
              error_code: :no_error
            }
            | _
          ]
        } ->
          {204, nil}

        %KafkaEx.Protocol.DeleteTopics.Response{
          topic_errors: [
            %KafkaEx.Protocol.DeleteTopics.TopicError{
              error_code: err
            }
            | _
          ]
        } ->
          {500, %{"error" => Utilities.to_string(err)}}
      end

    rtn = response_body(st_time, json_response)
    conn |> send_response(status, rtn)
  end

  def create_topic(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()
    topic = params["topic"]
    num_partitions = params["num_partitions"] || 1
    replication_factor = params["replication_factor"] || 1

    topic_request = %KafkaEx.Protocol.CreateTopics.TopicRequest{
      topic: topic,
      num_partitions: num_partitions,
      replication_factor: replication_factor
    }

    r = KafkaEx.create_topics([topic_request])
    Logging.debug("creation topic result:~p", [r])

    {status, json_response} =
      case r do
        %KafkaEx.Protocol.CreateTopics.Response{
          topic_errors: [
            %KafkaEx.Protocol.CreateTopics.TopicError{
              error_code: :no_error
            }
            | _
          ]
        } ->
          {204, nil}

        %KafkaEx.Protocol.CreateTopics.Response{
          topic_errors: [
            %KafkaEx.Protocol.CreateTopics.TopicError{
              error_code: err
            }
            | _
          ]
        } ->
          {500, %{"error" => Utilities.to_string(err)}}
      end

    rtn = response_body(st_time, json_response)
    conn |> send_response(status, rtn)
  end
end
