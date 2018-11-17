defmodule GatewayCore.Outputs.IRMTN.SMS do
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

  def build_sms(
        id,
        short_code,
        linkid,
        service_id,
        spid,
        to,
        body,
        sms_type = "send_sms",
        module_name = nil,
        function_name = nil,
        arguments = nil
      ) do
    %DatabaseEngine.Models.SMS{
      id: id,
      receiver: to,
      body: body,
      options: %{
        "sms_type" => sms_type,
        "irmtn_short_code" => short_code,
        "irmtn_linkid" => linkid,
        "irmtn_service_id" => service_id,
        "irmtn_spid" => spid
      },
      internal_callback:
        if module_name do
          %DatabaseEngine.Models.InternalCallback{
            module_name: module_name,
            function_name: function_name,
            arguments: arguments
          }
        else
          nil
        end
    }
  end

  @spec send_sms_to_gw(%DatabaseEngine.Models.SMS{}) :: boolean()
  def send_sms_to_gw(
        sms_to_send = %DatabaseEngine.Models.SMS{
          id: id,
          internal_callback: internal_callback,
          options: %{"sms_type" => sms_type}
        }
      ) do
    sms_type =
      if is_atom(sms_type) do
        sms_type
      else
        sms_type |> String.to_atom()
      end

    timeout = @gateway_config |> GWConf.get_timeout(sms_type)
    {:ok, body_to_send, action, _} = get_sms_post_data(sms_to_send)
    url = @gateway_config |> GWConf.get_call_url(sms_type)
    headers = @gateway_config |> GWConf.get_request_headers(id, @basic_auth, [])

    {:ok, status_code, resp_headers, resp_body} =
      Networking.HTTP1_1.wsdl(url, action, headers, body_to_send, timeout, sms_to_send.id)

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

  defp get_sms_post_data(
         sms = %DatabaseEngine.Models.SMS{
           :body => body,
           :id => id,
           :receiver => address,
           :options => %{
             "irmtn_short_code" => short_code,
             "irmtn_linkid" => linkid,
             "irmtn_service_id" => service_id,
             "irmtn_spid" => spid
           }
         }
       ) do
    template =@gateway_config |> GWConf.template(:send_sms)
    action = @gateway_config |> GWConf.action(:send_sms)

    {:ok,
     template
     |> EEx.eval_string(
       short_code: short_code,
       service_id: service_id,
       spid: spid,
       linkid: linkid,
       correlator: id,
       address: address,
       message: body
     ), action, sms}
  end
end
