defmodule GatewayCore.Outputs.IMI do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require Utilities
  @gateway_config Application.get_env(:gateway_core, Red9Cobra.IMI)

  use GatewayCore.Outputs.Red9CobraSimpleOutGW
  # require IEx

  def gw_init() do
    Logging.debug("Called")
    []
  end

  def gw_queue_list() do
    Logging.debug("Called")

    r = [
      in_Q: @gateway_config[:input_Q],
      success_Q: @gateway_config[:success_Q],
      fail_Q: @gateway_config[:fail_Q]
    ]

    Logging.debug("Returns:~p", [r])
    r
  end

  def send_sms_list(sms_list_to_send, state) do
    Logging.debug("Called.")
    results = sms_list_to_send |> Enum.map(&GatewayCore.Outputs.IMI.SMS.send_sms_to_gw/1)
    r = {state, results}
    Logging.debug("Retuens:~p", [r])
    r
  end

  def send_otp_list(otp_list, state) do
    Logging.debug("Called")
    results = otp_list |> Enum.map(&GatewayCore.Outputs.IMI.OTP.otp_req_to_gw/1)
    r = {state, results}
    Logging.debug("Returns:~p", [r])
    r
  end

  def send_charge_list(charge_list, state) do
    Logging.debug("Called")
    Logging.error("NO CHARGE METHOD DEFINED FOR ~p MODULE, BUT SOMEONE ARE SENDING CHARGE !!! CHARGE LIST:~p",[__MODULE__,charge_list])
    r = {state, charge_list|> Enum.map(fn _ -> false end)}
    Logging.debug("Returns:~p", [r])
    r
  end

end
