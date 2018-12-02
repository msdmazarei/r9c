defmodule Dispatcher.Process.VAS.UserProcess do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require Utilities

  alias Utilities.Logging
  alias :luerl, as: LUA

  use GenServer

  @wait_to_new_message_timeout_to_hibernate 15_000
  @wait_to_new_message_timeout_to_terminate 60_000
  @check_last_arrived_message_time :check_last_arrived_message_time

  defmodule State do
    {
      defstruct(
        cell_no: nil,
        last_arrived_message_time: nil,
        lua_state: nil,
        cel_script_limits: %{
          run_timeout: 20_000,
          http_call_limit: 20
        }
      )
    }
  end

  def start_link(state, opts) do
    GenServer.start_link(__MODULE__, state, opts)
  end

  def init(%{"cell_no" => cell_no}) do
    watchdog()
    {:ok, %State{cell_no: cell_no, last_arrived_message_time: Utilities.now()}}
    #    {:ok, %{"last_arrived_message_time" => Utilities.now(), "cell_no" => cell_no}}
  end

  def handle_call(:alive, _, state) do
    {:reply, true, state}
  end

  def handle_call(
        {:echo, msg},
        _,
        state = %State{last_arrived_message_time: last_arrived_message_time, cell_no: cell_no}
      ) do
    Logging.debug("~p Called, Echo Message:~p", [cell_no, msg])
    state = %State{state | last_arrived_message_time: Utilities.now()}
    {:reply, msg, state}
  end

  def handle_call(
        {:ingress, sms = %DatabaseEngine.Models.SMS{sender: cell_no, receiver: service_no}},
        _from,
        state = %State{cell_no: cell_no}
      ) do
    Logging.debug("ingress sms :~p arrived", [sms])
    Logging.debug("~p get service definition for ~p", [cell_no, service_no])

    state = %State{state | last_arrived_message_time: Utilities.now()}
    {:reply, :ok, state}
  end

  def handle_call(
        msg,
        _,
        state = %State{cell_no: cell_no, last_arrived_message_time: last_arrived_message_time}
      ) do
    Logging.debug("~p: got msg:~p but have no plan to do what. ignore it", [cell_no, msg])
    state = %State{state | last_arrived_message_time: Utilities.now()}
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(
        :timeout,
        state = %State{last_arrived_message_time: last_arrived_message_time, cell_no: cell_no}
      ) do
    Logging.debug("~p :timeout called. last_arrived_message_time:~p", [
      cell_no,
      last_arrived_message_time
    ])

    if Utilities.now() - last_arrived_message_time > @wait_to_new_message_timeout_to_hibernate do
      Logging.debug("~p Enter in Hibernate Mode. Current State:~p", [cell_no, state])
      :proc_lib.hibernate(:gen_server, :enter_loop, [__MODULE__, [], state])
    end

    Logging.debug("~p All thing is right, let try hibernating later.", [cell_no])
    Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)
    {:noreply, state}
  end

  def handle_info(
        @check_last_arrived_message_time,
        state = %State{last_arrived_message_time: last_arrived_message_time, cell_no: cell_no}
      ) do
    Logging.debug(
      "~p Called. msg:~p With Params Last Arrived Message:~p",
      [cell_no, @check_last_arrived_message_time, last_arrived_message_time]
    )

    if Utilities.now() - last_arrived_message_time >= @wait_to_new_message_timeout_to_terminate do
      Logging.debug("~p: Stop process:~p cause of no message after long time, So let stop it", [
        cell_no,
        self()
      ])

      Process.exit(self(), :normal)
    end

    Process.send_after(
      self(),
      @check_last_arrived_message_time,
      @wait_to_new_message_timeout_to_terminate + 100
    )

    {:noreply, state}
  end

  @compile {:inline, watchdog: 0}

  defp watchdog() do
    Process.send_after(self(), :timeout, @wait_to_new_message_timeout_to_hibernate + 100)

    Process.send_after(
      self(),
      @check_last_arrived_message_time,
      @wait_to_new_message_timeout_to_terminate + 100
    )
  end
end
