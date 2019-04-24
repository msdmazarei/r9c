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
    all = Repo.get_all()|>Enum.map(fn x -> x|> Map.from_struct  end)

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


end
