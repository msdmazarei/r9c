defmodule GatewayCore.Utils.Helper do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def message_out_gateway(message = %DatabaseEngine.Models.SMS{options: %{"gateway" => gateway}}) do
    Logging.debug("Called")

    r =
      case gateway |> String.downcase() do
        "dummy" ->
          GatewayCore.Outputs.Dummy

        "imi" ->
          GatewayCore.Outputs.Dummy

        "irmtn" ->
          GatewayCore.Outputs.IRMTN.SMS

        _ ->
          Logging.warn("UNKOWN GATEWAY FOR SMS RETURNS. sms:~p", [message])
          UNKOWN_GATEWAY
      end

    Logging.debug("return gateway:~p", [r])
    r
  end

  def message_out_gateway(message = %DatabaseEngine.Models.SMS{}) do
    Logging.debug("Called With bad args:~p", [message])
    UNKNOWN_GATEWAY
  end
end
