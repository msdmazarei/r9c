defmodule Dispatcher.Process.VAS.UserProcess do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  use GenServer

  def start_link(state, opts) do
    GenServer.start_link(__MODULE__, state, opts)
  end

  def init(_opts) do
    {:ok, %{}}
  end

  def handle_call(:alive, _, state) do
    {:reply, true, state}
  end

  def handle_call({:echo, msg}, _, state) do
    Logging.debug("Called, Echo Message:~p", [msg])
    {:reply, msg, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end
end
