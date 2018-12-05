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
    # will supervisor by genral consumer supervisors
    GatewayCore.Outputs.Dummy.start()
    GatewayCore.Outputs.IMI.start()
    GatewayCore.Outputs.IrMtn.start()

    run_gsm_modems_gateway()
    rtn
  end

  def run_gsm_modems_gateway() do
    Logging.debug("Called node:~p", [to_string(node())])
    c = Application.get_env(:gateway_core, GatewayCore.Drivers.GsmModemDriver.Output)
    node_config = c[node()]
    Logging.debug("Node Config: ~p", [node_config])

    if node_config == nil do
      true
    else
      for modem <- node_config[:modems] do
        add_gsm_modem(modem)
      end

      GatewayCore.Drivers.GsmModemDriver.Output.start()

      if node_config[:ingress_Q] != nil do
        for {_, p, _, _} <-
              DynamicSupervisor.which_children(GatewayCore.GSMModemGateway.Supervisor) do
          :simple_gsm_modem_over_tcp.register_to_inform_every_message(
            p,
            GatewayCore.Drivers.GsmModemDriver.Input,
            :receive_sms,
            [node_config[:ingress_Q]]
          )
        end
      end
    end

    #
    #    Logging.debug("config for modems are:~p", [Application.get_env(:gateway_core, :modems)])
    #
    #    for modem <- Application.get_env(:gateway_core, :modems) do
    #      add_gsm_modem(modem)
    #    end
    #
    #    gsm_gateway = Application.get_env(:gateway_core, :gsm_gateway)
    #    Logging.debug("Config for gsm_gateway:~p", [gsm_gateway])
    #
    #    if gsm_gateway != nil do
    #      Logging.debug("gsm_gateway is not nil so check output queue, output queue: ~p", [
    #        gsm_gateway[:q_out]
    #      ])
    #
    #      if gsm_gateway[:q_out] != nil do
    #        Logging.debug("gsm_gateway has output queue so start its consumer...")
    #
    #        DatabaseEngine.DurableQueue.start_consumer_group(
    #          gsm_gateway[:q_out],
    #          gsm_gateway[:q_out] <> "_consumer",
    #          GatewayCore.Drivers.GsmModemDriver.Output
    #        )
    #      end
    #
    #      if gsm_gateway[:q_in] != nil do
    #        Logging.debug(
    #          "gsm_gateway has input queue so register to all incomping messages from modems..."
    #        )
    #
    #        for {_, p, _, _} <-
    #              DynamicSupervisor.which_children(GatewayCore.GSMModemGateway.Supervisor) do
    #          :simple_gsm_modem_over_tcp.register_to_inform_every_message(
    #            p,
    #            GatewayCore.Drivers.GsmModemDriver.Input,
    #            :receive_sms,
    #            [gsm_gateway[:q_in]]
    #          )
    #        end
    #      end
    #    end
  end

  def add_gsm_modem({address, port}) do
    Logging.debug("Called address:~p,port:~p", [address, port])

    child_spec = %{
      :id => "modem-#{address}-#{port}",
      :start => {:simple_gsm_modem_over_tcp, :start_link, [:binary.bin_to_list(address), port]},
      :restart => :permanent,
      :type => :worker
    }

    DynamicSupervisor.start_child(GatewayCore.GSMModemGateway.Supervisor, child_spec)
  end
end
