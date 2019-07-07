defmodule HttpServers.DynamicSup do 
  use DynamicSupervisor

  def start_link(init_args) do
    DynamicSupervisor.start_link(__MODULE__, init_args,name: __MODULE__)
  end

  @impl true
  def init(_init_args) do 
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end

