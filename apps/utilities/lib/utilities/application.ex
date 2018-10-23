defmodule Utilities.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies)

    children = [
      {Cluster.Supervisor, [topologies, [name: Red9Cobra.ClusterSupervisor]]}
      # Starts a worker by calling: Utilities.Worker.start_link(arg)
      # {Utilities.Worker, arg},
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Utilities.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
