defmodule  HttpServers.ProgramableHttpServer do

  import Plug.Conn

  def init(options) do
    # initialize options
    options
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello world")
  end

  def start_server(options) do
    Plug.Cowboy.http HttpServers.ProgramableHttpServer, [], options
  end
end
