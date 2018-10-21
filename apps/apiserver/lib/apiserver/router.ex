defmodule Apiserver.Router do
  use Apiserver, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", Apiserver do
    pipe_through :api
  end
end
