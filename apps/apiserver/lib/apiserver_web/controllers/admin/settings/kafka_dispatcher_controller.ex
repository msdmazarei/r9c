defmodule ApiserverWeb.Admin.Settings.Kafka.Dispatcher.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities

  require DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher
  alias DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher, as: Model

  require DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher.Repo
  alias DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher.Repo

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

  def list_all(conn, _params) do
    st_time = Utilities.now()

    Logging.debug("called. ")
    all = Repo.get_all() |> Enum.map(fn x -> x |> Map.from_struct() end)

    rtn = response_body(st_time, all)

    Logging.debug("returns: ~p", [rtn])
    Logging.info("Done.")

    conn
    |> json(rtn)
  end

  def add_new(conn, params) do
    st_time = Utilities.now()
    Logging.debug("called. params:~p", [params])

    model_instance = params_to_struct(params)

    db_result =
      try do
        Repo.add_new(model_instance)
      catch
        {:aborted, v} ->
          {:aborted, v}
      end

    Logging.debug("db_result:~p", [db_result])

    {status, rtn} =
      case db_result do
        {:aborted, :already_exists} ->
          Logging.info("item was already registered.")

          {409,
           %{
             "host" => "uniq"
           }}

        {:aborted, :invalid_model} ->
          Logging.info("bad request data")

          {400,
           %{
             "request_body" => "bad model"
           }}

        {:atomic, model} ->
          Logging.info("successfully done")
          {200, model |> Map.from_struct()}

        {:aborted, e} ->
          Logging.error("unpredictaed excpetion happend. error:~p", [e])
          {500, %{"error" => "unhandled error"}}
      end

    result = response_body(st_time, rtn)

    conn |> send_response(status, result)
  end

  def del(conn, params) do
    st_time = Utilities.now()
    Logging.debug("Called. params:~p", [params])
    key = params["key"]

    {status, rtn} =
      case Repo.del(key) do
        {:atomic, n} ->
          Logging.info("deleted successfully. broker:~p", [key])
          {200, n |> Map.from_struct()}

        {:aborted, :not_exist} ->
          Logging.info("not exists. broker:~p", [key])
          {404, nil}

        {:aborted, e} ->
          Logging.error("unhandled excpetion:~p", [e])
          {500, %{"error" => "unhandled error"}}
      end

    result = response_body(st_time, rtn)

    conn |> send_response(status, result)
  end

  def edit(conn, params) do
    st_time = Utilities.now()

    Logging.debug("called. params:~p", [params])
    model_instance = params_to_struct(params)

    {status, rtn} =
      case Repo.edit(model_instance) do
        {:aborted, :conflict} ->
          Logging.info("version conflict. broker:~p", [model_instance.q_name])
          {409, %{"version" => "conflict"}}

        {:aborted, :not_exist} ->
          Logging.info("node not exists. broker:~p", [model_instance.q_name])
          {404, nil}

        {:aborted, :invalid_model} ->
          Logging.info("bad request body to edit broker:~p", [model_instance.q_name])
          {400, %{"body" => "invalid_request"}}

        {:aborted, e} ->
          Logging.error("unhandled request. error:~p", [e])
          {500, %{"error" => "unhandled error"}}

        {:atomic, model} ->
          Logging.info("successfully edited. broker:~p", [model_instance.q_name])
          {200, model |> Map.from_struct()}
      end

    result = response_body(st_time, rtn)

    conn |> send_response(status, result)
  end

  def get(conn, params) do
    st_time = Utilities.now()

    key = params["key"]

    {status, rtn} =
      case Repo.get(key) do
        nil ->
          Logging.info("not found. broker:~p", [key])
          {404, nil}

        db_instance when is_map(db_instance) ->
          {200, db_instance |> Map.from_struct()}

        e ->
          Logging.error("unhandled response:~p", [e])
          {500, %{"error" => "unhandled response"}}
      end

    result = response_body(st_time, rtn)

    conn |> send_response(status, result)
  end

  def is_dispatcher_running(dispatcher) do
    # consumer_name: "",
    #         q_name: "",
    #         dispatcher_node_name: "",
    #         description: "",
    #         version: 0,
    #         additional_props: %{}
    Logging.debug("called. dispatcher:~p", [dispatcher])

    node = dispatcher.dispatcher_node_name |> String.to_atom()
    t = Utilities.remote_async_task(node, DatabaseEngine.DurableQueue, :get_running_consumers, [])
    r = Utilities.await_remote_task(t, 5000, :no_response)

    if is_map(r) do
      {r |> Map.has_key?(dispatcher.consumer_name), r}
    else
      :no_response
    end
  end

  def start_consumer(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()

    key = params["key"]

    {status, json_response} =
      case Repo.get(key) do
        nil ->
          Logging.info("not found. broker:~p", [key])
          {404, nil}

        db_instance when is_map(db_instance) ->
          dispatcher = db_instance |> Map.from_struct()

          case is_dispatcher_running(dispatcher) do
            :no_response ->
              {500, "node not respond"}

            {true, _} ->
              {201, "already running"}

            {false, _} ->
              consumer_module = dispatcher.additional_props || %{}

              consumer_module =
                Map.get(consumer_module, "consumer_module") ||
                  to_string(Dispatcher.Consumers.InQConsumer)

              consumer_module = consumer_module |> String.to_atom()

              t =
                Utilities.remote_async_task(
                  dispatcher.dispatcher_node_name |> String.to_atom(),
                  DatabaseEngine.DurableQueue,
                  :start_consumer_group,
                  [dispatcher.q_name, dispatcher.consumer_name, consumer_module]
                )

              r = Utilities.await_remote_task(t, 5000, :no_response)

              case r do
                :no_response -> {500, "node not respond"}
                {:ok, _} -> {200, "started"}
                {:ok, _, _} -> {200, "started"}
                {:error, e} -> {500, Utilities.to_string(e)}
                :ignore -> {500, "ignore"}
              end
          end

        e ->
          Logging.error("unhandled response:~p", [e])
          {500, %{"error" => "unhandled response"}}
      end

    conn |> send_response(status, response_body(st_time, json_response))
  end

  def stop_consumer(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()

    key = params["key"]

    {status, json_response} =
      case Repo.get(key) do
        nil ->
          Logging.info("not found. broker:~p", [key])
          {404, nil}

        db_instance when is_map(db_instance) ->
          dispatcher = db_instance |> Map.from_struct()

          case is_dispatcher_running(dispatcher) do
            :no_response ->
              {500, "node not respond"}

            {true, running_dispatchers} ->
              first_workder_pid = running_dispatchers[key] |> hd() |> Map.get("pid")
              Logging.debug("fisrt workder pid:~p", [first_workder_pid])
              #
              t =
                Utilities.remote_async_task(
                  dispatcher.dispatcher_node_name |> String.to_atom(),
                  DatabaseEngine.DurableQueue,
                  :stop_consumer_greoup_by_workder_pid,
                  [first_workder_pid]
                )

              r = Utilities.await_remote_task(t, 5000, :no_response)
              Logging.debug("remote task result:~p", [r])

              case r do
                :no_response ->
                  {500, "node not respond"}

                _ ->
                  {200, "ok"}
              end

            {false, _} ->
              {201, "already stopped"}
          end

        e ->
          Logging.error("unhandled response:~p", [e])
          {500, %{"error" => "unhandled response"}}
      end

    conn |> send_response(status, response_body(st_time, json_response))
  end

  def running_consumers(conn, params) do
    Logging.debug("Called. params:~p", [params])
    st_time = Utilities.now()
    nodes = (params["nodes"] || [:erlang.node() |> to_string()]) |> Enum.map(&String.to_atom/1)

    tasks =
      nodes
      |> Enum.map(fn x ->
        Utilities.remote_async_task(x, DatabaseEngine.DurableQueue, :get_running_consumers, [])
      end)

    results = Utilities.await_multi_task(tasks, 5000, %{})

    node_result =
      Enum.zip(nodes, results)
      |> Map.new()
      |> Utilities.for_each_non_iterable_item(fn x ->
        if is_pid(x) do
          x |> :erlang.pid_to_list() |> to_string
        else
          x
        end
      end)

    json_response = response_body(st_time, node_result)
    conn |> send_response(200, json_response)
  end

  def consumer_stats(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()
    nodes = params["nodes"] || Utilities.all_active_nodes() |> Enum.map(&to_string/1)
    nodes = nodes |> Enum.map(&String.to_atom/1)

    tasks =
      nodes
      |> Enum.map(fn x ->
        Utilities.remote_async_task(
          x,
          DatabaseEngine.DurableQueue,
          :get_running_consumer_stats,
          []
        )
      end)

    results = Utilities.await_multi_task(tasks, 5000, :no_response)
    node_result = Enum.zip(nodes, results) |> Map.new()

    rtn = response_body(st_time, node_result)
    conn |> send_response(200, rtn)
  end
end
