defmodule ApiserverWeb.DummyGatewayIngressController do
  use ApiserverWeb, :controller

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def hasan(conn, params) do
    Logging.debug("~p", [params])
    conn |> json(%{message: params["p"]})
  end
end
