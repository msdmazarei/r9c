defmodule ApiserverWeb.Client.ClientController do
  @moduledoc false

  use ApiserverWeb.RCBaseController, :crud

  alias ApiserverWeb.RCBaseController

  plug :request_body_validation

  def all_clients(conn, params) do
    conn |> json(%{message: __ENV__.file})
  end

  def get(id, version) do
  end

  def create(data) do
  end

  def edit(id, data) do
  end

  def delete(id) do
  end
end
