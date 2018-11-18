defmodule Apiserver.MonitoringController do
  use Apiserver, :controller

  def ping(conn, _params) do
    json(conn, %{response: :pong})
  end
end
