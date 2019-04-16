defmodule BootStraper.Pinger.GServer do
  use GenServer
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.SystemConfig
  alias DatabaseEngine.Interface.SystemConfig

  @ping_interval 5_000
  @update_config_interval 60_000
  @ping_tref "ping_tref"
  @all_nodes_to_ping "all_nodes_to_ping"
  @update_config_tref "update_config_tref"

  def start_link(state) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  @impl true
  def init(init_arg) do
    Logging.debug("Called. init args:~p", [init_arg])
    # get list of all nodes
    all_nodes =
      SystemConfig.Node.Repo.list_all_nodes()
      |> Enum.map(fn x -> x.node_name |> String.to_atom() end)

    {:ok, ping_tref} = :timer.send_interval(@ping_interval, :ping_them)
    {:ok, update_config_tref} = :timer.send_interval(@update_config_interval, :update_config)

    {:ok,
     %{
       @all_nodes_to_ping => all_nodes,
       @ping_tref => ping_tref,
       @update_config_tref => update_config_tref
     }}
  end

  @impl true
  def terminate(
        reason,
        state = %{
          @ping_terf => ping_terf
        }
      ) do
    Logging.info("called. reason:~p", [reason])
    :timer.cancel(ping_terf)
    state
  end

  @imple true
  def handle_info(:update_config, state) do
    all_nodes =
      SystemConfig.Node.Repo.list_all_nodes()
      |> Enum.map(fn x -> x.node_name |> String.to_atom() end)

    new_state = state |> Map.put(@all_nodes_to_ping, all_nodes)
    {:noreply, new_state}
  end

  @impl true
  def handle_info(
        :ping_them,
        state = %{
          @all_nodes_to_ping => all_nodes_to_ping
        }
      ) do
    current_active_nodes = Utilities.all_active_nodes()

    absences =
      all_nodes_to_ping
      |> Enum.filter(fn x ->
        r = current_active_nodes |> Enum.member?(x)
        !r
      end)

    absences
    |> Enum.map(fn x ->
      :net_adm.ping(x)
    end)

    {:noreply, state}
  end
end

defmodule BootStraper.Pinger.Supervisour do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, %{}, opts)
  end

  @impl true
  def init(args) do
    children = [
      {BootStraper.Pinger.GServer, name: Pinger }
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
