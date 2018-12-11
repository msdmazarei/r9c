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
