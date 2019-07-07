defmodule ApiserverWeb.ESB.MainController do
  require Logger
  require  Utilities.Logging
  alias Utilities.Logging
  require Plug.Conn
  alias Plug.Conn


  import Phoenix.Controller




  use ApiserverWeb, :controller

  def first_action(conn,params) do
    Logging.debug("hello")
    {:ok, body, _conn} = Plug.Conn.read_body(conn)
    Logging.debug("body:~p",[body])
    Logging.debug("headers:~p",[conn.req_headers])
    conn |> send_resp(200,"helloo")
  end
end
