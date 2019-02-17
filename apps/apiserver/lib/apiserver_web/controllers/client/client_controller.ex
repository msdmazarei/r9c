defmodule ApiserverWeb.Client.ClientController do
  @moduledoc false

  use ApiserverWeb.RCBaseController, :crud

  alias ApiserverWeb.RCBaseController

  plug :request_body_validation

  require DatabaseEngine.MnesiaWrapper
  alias DatabaseEngine.MnesiaWrapper

  require DatabaseEngine.Repositories.Mnesia.Client
  alias DatabaseEngine.Repositories.Mnesia.Client, as: Repo

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.DbModels.Client, as: Model

  def all_clients(conn, params) do
    conn
    |> json(%{message: __ENV__.file})
  end

  def list(conn, params) do
    {from_date, _} = params["from_date"] |> Integer.parse()
    {count, _} = params["count"] |> Integer.parse()
  end

  def get(conn, params) do
    Logging.debug("Called.")
    id = params["id"]
    Logging.debug("get instance with id:~p", [id])

    case Repo.get_by_id(id) do
      nil ->
        Logging.debug("id: ~p not found. 404.", [id])

        conn
        |> put_status(404)
        |> text("not found.")

      instance ->
        Logging.debug("instance id: ~p is found.", [id])

        conn
        |> json(instance)
    end
  end

  def create(conn, _) do
    Logging.debug("Called.")
    req_body = conn.body_params
    Logging.debug("body is: ~p", [req_body])

    db_op_result =
      MnesiaWrapper.do_transactional(fn ->
        Repo.add_new(req_body["name"], req_body["is_company"], false)
      end)

    case db_op_result do
      {true, instance} ->
        Logging.info(
          "new client added. id: ~p",
          [instance.id]
        )

        conn
        # |> put_resp_content_type("application/json")
        |> json(instance)

      {false, error} ->
        Logging.error("problem to add new client. err:~p", [error])

        conn
        |> send_resp(500, "no_idea")
    end
  end

  def edit(conn, params) do
    Logging.debug("Called.")
    instance_id = params["id"]

    {instance_version, _} =
      params["version"]
      |> Integer.parse()

    Logging.debug("editing id:~p version:~p", [instance_id, instance_version])
    req_body = conn.body_params

    db_op_result =
      MnesiaWrapper.do_transactional(fn ->
        case Repo.get_by_id(instance_id) do
          nil ->
            nil

          db_instance ->
            if db_instance.version != instance_version do
              :bad_version
            else
              Repo.update(db_instance, req_body)
            end
        end
      end)

    case db_op_result do
      {true, nil} ->
        Logging.debug("no model found in db. for id:~p", [instance_id])

        conn
        |> put_status(404)
        |> json(%{})

      {true, :bad_version} ->
        Logging.debug("bad version")

        conn
        |> put_status(409)
        |> json(%{})

      {true, result} ->
        Logging.debug("update is done successfully")

        conn
        |> json(result)

      {false, err} ->
        Logging.debug("error is happend in update process. error:~p", [err])

        conn
        |> put_status(500)
        |> json(%{})
    end
  end

  def delete(conn, params) do
    Logging.debug("Called.")
    instance_id = params["id"]

    {instance_version, _} =
      params["version"]
      |> Integer.parse()

    Logging.debug("Deleting id:~p version:~p", [instance_id, instance_version])

    {true, result} =
      MnesiaWrapper.do_transactional(fn ->
        Repo.delete(instance_id, instance_version)
      end)

    case result do
      nil ->
        conn
        |> put_status(404)
        |> json(%{})

      :bad_version ->
        conn
        |> put_status(409)
        |> json(%{"version" => "confilicts"})

      true ->
        conn
        |> put_status(204)
        |> json(%{})

      false ->
        conn
        |> put_status(500)
        |> json(%{})
    end
  end
end
