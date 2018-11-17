defmodule GatewayCore.Outputs.IRMTN.Charge do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require Exml
  alias Utilities.Logging
  require GatewayCore.Utils.VAS.GW
  alias GatewayCore.Utils.VAS.GW, as: GWConf

  @gateway_config Application.get_env(:gateway_core, Red9Cobra.IRMTN)
  @error_codes @gateway_config[:error_codes]
  #  @default_max_daily_charge_sum @gateway_config[:max_user_charge_per_day]
  #  @default_max_daily_charge_retry_count @gateway_config[:max_user_charge_retry_count]
#  @wsdl_action_endpoint @gateway_config[:wsdl_action_endpoint]
  @basic_auth @gateway_config[:sms_center_auth]

  def charge(
        charge_request = %DatabaseEngine.Models.Charge.VAS{
          id: id,
          internal_callback: internal_callback
        }
      ) do
    {:ok, body_to_send, action} = get_charge_post_data(charge_request)
    url = @gateway_config |> GWConf.get_call_url(:charge)
    timeout = @gateway_config |> GWConf.get_timeout(:charge)
    headers = @gateway_config |> GWConf.get_request_headers(id, @basic_auth)

    {:ok, status_code, resp_headers, resp_body} =
      Networking.HTTP1_1.wsdl(url, action, headers, body_to_send, timeout, charge_request.id)

    callback_data = %{
      :http => %{
        :headers => resp_headers,
        :body => resp_body,
        :status_code => status_code
      },
      :success => status_code > 199 and status_code < 300
    }

    {callback_data, rtn} =
      case status_code do
        200 ->
          Logging.info("Message Sent, id:~p", [id])
          {callback_data, true}

        500 ->
          Logging.warn("Error From MNO for message id: ~p", [id])

          GatewayCore.Utils.VAS.GW.irmtn_process_failed_response(
            resp_body,
            @error_codes,
            callback_data
          )

        _ ->
          GatewayCore.Utils.callback(callback_data, internal_callback)
          Logging.warn("Message Sending Failed id:~p", [id])
          {callback_data, false}
      end

    GatewayCore.Utils.callback(callback_data, internal_callback)
    Logging.debug("returns: ~p", [rtn])
    rtn
  end

  defp get_charge_post_data(%DatabaseEngine.Models.Charge.VAS{
         service_id: service_id,
         id: id,
         contact: contact,
         charge_amount: charge_amount,
         options: %{spid: spid}
       }) do
    template = @gateway_config|> GWConf.template(:charge)
    action = @gateway_config |> GWConf.action(:charge)

    {:ok,
     template
     |> EEx.eval_string(
       service_id: service_id,
       spid: spid,
       correlator: id,
       address: contact,
       charge_amount: charge_amount
     ), action}
  end
end
