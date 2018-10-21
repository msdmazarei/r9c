defmodule Apiserver.Router do
  use Apiserver, :router
  @api_version 2

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/api/v#{@api_version}/monitoring", Apiserver do
    pipe_through(:api)
    get("/ping", MonitoringController, :ping)
  end
end
