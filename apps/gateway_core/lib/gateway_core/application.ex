defmodule GatewayCore.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: GatewayCore.Worker.start_link(arg)
      # {GatewayCore.Worker, arg},
      {GatewayCore.GSMModemGateway.Supervisor, []}

    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GatewayCore.Supervisor]
    rtn = Supervisor.start_link(children, opts)
    run_gsm_modems_gateway()
    rtn

  end

  def run_gsm_modems_gateway() do
    Logging.debug("config for modems are:~p", [Application.get_env(:gateway_core, :modems)])
    for modem <- Application.get_env(:gateway_core, :modems) do
      add_gsm_modem(modem)
    end

    gsm_gateway  = Application.get_env(:gateway_core,:gsm_gateway)
    Logging.debug("Config for gsm_gateway:~p",[gsm_gateway])
    if gsm_gateway != :nil do
      Logging.debug("gsm_gateway is not nil so check output queue, output queue: ~p",[ gsm_gateway[:q_out] ])
        if gsm_gateway[:q_out] != :nil do
          Logging.debug("gsm_gateway has output gateway so start its consumer...")
          DatabaseEngine.DurableQueue.start_consumer_group(
            gsm_gateway[:q_out] ,
            gsm_gateway[:q_out] <> "_consumer" ,
            GatewayCore.Drivers.GsmModemDriver.Output
          )
        end
    end


    for {_, p, _, _} <- DynamicSupervisor.which_children(GatewayCore.GSMModemGateway.Supervisor) do
      #      register_handler(Pid, SENDER_Regex, TXTRegex, SUCCESSMODULE, SUCCESSFUNC, SUCCESSARGS, FAILMODULE, FAILFUNC, FAILARGS, TIMEOUT) ->
      :simple_gsm_modem_over_tcp.register_handler(
        p,
        :binary.bin_to_list("989360076133"),
        :binary.bin_to_list("salam"),
        GatewayCore.Drivers.GsmModemDriver,
        :sms_received,[],
        :erlang,
        :fwrite,[],
        1999999999
      )
    end

  end

  def add_gsm_modem({address, port}) do
    child_spec = %{
      :id => "modem-#{address}-#{port}",
      :start =>
        {:simple_gsm_modem_over_tcp, :start_link,
        [:binary.bin_to_list(address), port]},
      :restart => :permanent,
      :type => :worker
    }
    DynamicSupervisor.start_child(GatewayCore.GSMModemGateway.Supervisor, child_spec)
  end
end
