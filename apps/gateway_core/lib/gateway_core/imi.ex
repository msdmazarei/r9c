defmodule GatewayCore.Outputs.IMI do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  use GatewayCore.Outputs.Red9CobraSimpleOutGW
  # require IEx
  @gateway_config Application.get_env(:red9cobra, Red9Cobra.IMI)
  @wsdl_action_endpoint @gateway_config[:wsdl_action_endpoint]
  @basic_auth @gateway_config[:sms_center_auth]

  @spec get_send_sms_post_data(DatabaseEngine.Models.SMS) ::
          {:ok, String.t(), String.t(), DatabaseEngine.Models.SMS}
  defp get_send_sms_post_data(
         message = %DatabaseEngine.Models.SMS{
           options: %{
             :imi_charge_code => charge_code,
             :imi_short_code => short_code
           }
         }
       ) do
    Logging.debug("Called", [])
    send_sms_config = @gateway_config[:methods][:send_sms]
    template = send_sms_config[:template]
    action = (@wsdl_action_endpoint || "") <> (send_sms_config[:action] || "")

    delivery_endpoint =
      @gateway_config[:delivery_endpoint_scheme] <>
        "://" <> @gateway_config[:client_host] <> ":" <> @gateway_config[:client_port]

    Logging.debug("render template by SMS_MESSAGE:~p and DELIVERY_ENDPOINT:~p", [
      message,
      delivery_endpoint
    ])

    body_to_send =
      template
      |> EEx.eval_string(
        correlator: message.id,
        address: message.receiver,
        delivery_endpoint: delivery_endpoint,
        charge_code: charge_code,
        message: message.body,
        short_code: short_code
      )

    rtn = {:ok, body_to_send, action, message}
    Logging.debug("Returns:~p", [rtn])
    rtn
  end

  def gw_init() do
  end

  def gw_queue_list() do
  end

  def send_sms_list(_, _) do
  end
end
