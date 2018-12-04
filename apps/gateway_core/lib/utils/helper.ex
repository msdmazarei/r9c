defmodule GatewayCore.Utils.Helper do
  @moduledoc false

  def message_out_gateway(message = %DatabaseEngine.Models.SMS{options: %{"gateway" => gateway}}) do
    case gateway do
      "Dummy" -> GatewayCore.Outputs.Dummy
      "IMI" -> GatewayCore.Outputs.Dummy
      "IRMTN" -> GatewayCore.Outputs.IRMTN.SMS
    end
  end
end
