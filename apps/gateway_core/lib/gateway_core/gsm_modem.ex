defmodule GatewayCore.GSMModemGateway.Supervisor do
  @moduledoc false

  use DynamicSupervisor

  def start_link(arg) do
    DynamicSupervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end


defmodule GatewayCore.GSMModemGateway do
  def send_sms(target_no, body) do
    gsm_gateway = Application.get_env(:gateway_core, :gsm_gateway)
    DatabaseEngine.DurableQueue.enqueue(
      gsm_gateway[:q_out],
      %DatabaseEngine.Models.SMS{receiver: target_no, body: body}
    )
  end
end