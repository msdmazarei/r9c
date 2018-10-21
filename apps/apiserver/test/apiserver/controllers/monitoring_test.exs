defmodule Apiserver.MonitoringControllerTest do
  @api_version 2
  use Apiserver.ConnCase

  test "GET ping test", %{conn: conn} do
    conn = get conn, "/api/v#{@api_version}/monitoring/ping"
    assert json_response(conn, 200) == %{"response" => "pong"}
  end
end
