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

  scope "/api/admin/nodes", ApiserverWeb.Admin.Settings.Nodes do
    pipe_through(:api)
    get "/brief_actives", Controller, :brief_actives
    get "/actives", Controller, :list_active_nodes
    get "/actives/:nodename", Controller, :list_active_nodes_on_target_node
    get "/ping/:nodename", Controller, :ping
    post "/connect/:nodename", Controller, :connect_to_node
    post "/disconnect/:nodename", Controller, :disconnect_node
    get "/brief/:nodename", Controller, :node_brief
    post "/set_cookie", Controller, :set_cookie
    post "/transfer_modules/:nodename", Controller, :transfer_all_local_modules_to_target_node
    post "/save_cookie", Controller, :permanently_set_cookie_target_node
    get "/loaded_modules/:nodename", Controller, :get_all_loaded_modules
    get "/is_ready_to_run/:nodename", Controller, :check_is_node_proper_to_run_r9c
    post "/config", Controller, :add_new_node_to_config
    delete "/config/:nodename", Controller, :del_node_config
    put "/config", Controller, :edit_node_config
    get "/config/node/:nodename", Controller, :get_node_config
    get "/config/_all", Controller, :get_all_nodes_config
  end

  scope "/api/admin/mnesia", ApiserverWeb.Admin.Settings.Mnesia do
    pipe_through(:api)

    get "/status/:nodename", Controller, :get_mnesia_info_of_node
    get "/:nodename/table/:tbname", Controller, :get_mnesia_table_info_on_node
    post "/cluster/node/:nodename", Controller, :join_new_node
    delete "/cluster/node/:nodename", Controller, :del_cluster_node
    post "/cluster/table/:tablename/ram_copy/:nodename", Controller, :add_ram_replica_to_table
  end

  scope "/api/admin/kafka/runtime", ApiserverWeb.Admin.Settings.Kafka.Runtime do
    pipe_through(:api)

    post "/stop", Controller, :stop_kafka
    post "/start", Controller, :start_kafka
    post "/brokers", Controller, :get_brokers
    post "/topics", Controller, :get_topics
    delete "/topics/:topic", Controller, :delete_topic
    post "/topics/new", Controller, :create_topic
  end

  scope "/api/admin/kafka/brokers", ApiserverWeb.Admin.Settings.Kafka.Broker do
    pipe_through(:api)

    post "/", Controller, :add_new
    get "/:key", Controller, :get
    get "/", Controller, :list_all
    put "/", Controller, :edit
    delete "/:key", Controller, :del

    get "/run_time/queues", Controller, :list_kafka_queues
    get "/run_time/dispatchers/stats", Controller, :dispatchers_stat
  end

  scope "/api/admin/kafka/dispatchers", ApiserverWeb.Admin.Settings.Kafka.Dispatcher do
    get "/", Controller, :list_all
    post "/", Controller, :add_new
    put "/", Controller, :edit
    delete "/:key", Controller, :del
    get "/:key", Controller, :get
    post "/start/:key", Controller, :start_consumer
    post "/stop/:key", Controller, :stop_consumer
    post "/runtime/running_consumers", Controller, :running_consumers
    post "/runtime/stats", Controller, :consumer_stats

  end
end
