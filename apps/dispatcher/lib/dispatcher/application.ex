defmodule Dispatcher.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: Dispatcher.Worker.start_link(arg)
      # {Dispatcher.Worker, arg},
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Dispatcher.Supervisor]
    start_consumers()
    Supervisor.start_link(children, opts)
  end

  def start_consumers() do
    Logging.debug("called.")
    node_name = :erlang.node() |> to_string

    my_conf = DatabaseEngine.Interface.SystemConfig.Node.Repo.get_node(node_name)
    Logging.debug("node conf:~p", [my_conf])

    if my_conf do
      if my_conf.is_dispatcher_node do
        Logging.debug("I'm Dispatcher Node")
        all_dispatcher = DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher.Repo.get_all()

        my_q_dispatchers =
          all_dispatcher |> Enum.filter(fn x -> x.dispatcher_node_name == node_name end)

        Logging.debug("my queue dispatchers:~p", [my_q_dispatchers])

        res =
          my_q_dispatchers
          |> Enum.map(fn x ->
            DatabaseEngine.DurableQueue.start_consumer_group(
              x.q_name,
              x.consumer_name,
              Dispatcher.Consumers.InQConsumer
            )
          end)

        Enum.zip(my_q_dispatchers, res)
      else
        Logging.debug("I'm Not Dispatcher Node")
        []
      end
    end
  end

  def start_consumers_old() do
    input_queues = Application.get_env(:dispatcher, :input_queues)
    Logging.debug("config for input queues for dispatcher are:~p", [input_queues])

    (input_queues[node()] || [])
    |> Enum.map(fn q_name ->
      case DatabaseEngine.DurableQueue.start_consumer_group(
             q_name,
             "dispatcher_" <> q_name <> "_consumer",
             Dispatcher.Consumers.InQConsumer
           ) do
        {:error, {:already_started, _}} -> :ok
        v = {:error, _} -> throw(v)
        :ignore -> throw(:ignore)
        _ -> :ok
      end
    end)
  end
end
