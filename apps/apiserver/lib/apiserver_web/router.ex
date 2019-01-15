defmodule ApiserverWeb.Router do
  use ApiserverWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ApiserverWeb do
    pipe_through :browser

    get "/", PageController, :index
  end

  # Other scopes may use custom stacks.
  scope "/api", ApiserverWeb do
    pipe_through :api

    post "/hasan/:p", DummyGatewayIngressController, :hasan

    get "/admin/clients/:id", Client.ClientController, :get
    post "/admin/clients", Client.ClientController, :create
    delete "/admin/clients/:id/:version", Client.ClientController, :delete
    put "/admin/clients/:id/:version", Client.ClientController, :edit

  end
end
