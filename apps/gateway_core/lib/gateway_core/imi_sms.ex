defmodule GatewayCore.Outputs.IMI.SMS do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require Utilities

  @gateway_config Application.get_env(:gateway_core, Red9Cobra.IMI)
  @wsdl_action_endpoint @gateway_config[:wsdl_action_endpoint]
  @basic_auth @gateway_config[:sms_center_auth]

  @compile {:inline, callback: 2}
  @compile {:inline, get_delivery_base_url: 0}
  @compile {:inline, get_sms_center_base_url: 0}

  @spec send_sms_to_gw(%DatabaseEngine.Models.SMS{}) :: boolean()
  def send_sms_to_gw(
        sms_to_send = %DatabaseEngine.Models.SMS{
          options: %{:imi_service_key => service_key, :imi_sms_type => sms_type},
          internal_callback: internal_callback
        }
      ) do
    timeout = get_timeout(sms_type)
    {:ok, body_to_send, action, _} = get_send_sms_post_data(sms_to_send)
    url = get_call_url(sms_type)
    headers = get_request_headers(sms_to_send, service_key)

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

    callback(callback_data, internal_callback)

    if status_code > 199 && status_code < 300 do
      Logging.info("sms.id:~p sent successfully", [sms_to_send.id])
      true
    else
      Logging.warn("sms.id:~p sent failed, status_code:~p", [sms_to_send.id, status_code])
      false
    end
  end

  defp callback(_, nil) do
    :ok
  end

  defp callback(
         data,
         %DatabaseEngine.Models.InternalCallback{
           :module_name => module,
           :function_name => function,
           :arguments => arguments
         }
       ) do
    Utilities.callback(data, module, function, arguments)
  end

  @spec get_request_headers(%DatabaseEngine.Models.SMS{}, String.t()) ::
          Networking.HTTP1_1.headers()
  defp get_request_headers(sms, service_key) do
    basic_header =
      case @basic_auth do
        nil -> nil
        _ -> Networking.HTTP1_1.authotization_header(@basic_auth)
      end

    service_key_header = Networking.HTTP1_1.header("Servicekey", service_key)

    identifier_header =
      Networking.HTTP1_1.header(@gateway_config[:http_identifier_header], sms.id)

    headers =
      case basic_header do
        nil ->
          [service_key_header, identifier_header]

        _ ->
          [basic_header, service_key_header, identifier_header]
      end

    headers
  end

  @spec get_sms_center_base_url() :: String.t()
  defp get_sms_center_base_url() do
    @gateway_config[:sms_center_request_scheme] <>
      "://" <>
      (@gateway_config[:sms_center_host] |> Enum.random()) <>
      ":" <> @gateway_config[:sms_center_port]
  end

  @spec get_delivery_base_url() :: String.t()
  defp get_delivery_base_url() do
    Logging.debug("Called, delivery_endpoint_scheme:~p  client_host:~p client_port:~p ", [
      @gateway_config[:delivery_endpoint_scheme],
      @gateway_config[:client_host],
      @gateway_config[:client_port]
    ])

    @gateway_config[:delivery_endpoint_scheme] <>
      "://" <> @gateway_config[:client_host] <> ":" <> @gateway_config[:client_port]
  end

  @spec get_call_url(:send_sms) :: String.t()
  defp get_call_url(:send_sms) do
    get_sms_center_base_url() <> "/" <> @gateway_config[:methods][:send_sms][:call_method]
  end

  defp get_call_url(:send_sms_without_charge) do
    get_sms_center_base_url() <>
      "/" <> @gateway_config[:methods][:send_sms_without_charge][:call_method]
  end

  @spec get_timeout(:send_sms | :send_sms_without_charge) :: integer | nil
  defp get_timeout(sms_type) do
    @gateway_config[:methods][sms_type][:timeout] || @gateway_config[:http_timeout]
  end

  @spec get_send_sms_post_data(DatabaseEngine.Models.SMS) ::
          {:ok, String.t(), String.t(), DatabaseEngine.Models.SMS}
  defp get_send_sms_post_data(
         message = %DatabaseEngine.Models.SMS{
           options: %{
             :imi_short_code => short_code,
             :imi_sms_type => sms_type
           }
         }
       ) do
    charge_code = message.options[:imi_charge_code]
    Logging.debug("Called", [])
    send_sms_config = @gateway_config[:methods][sms_type]
    template = send_sms_config[:template]
    action = @wsdl_action_endpoint <> send_sms_config[:action]

    delivery_endpoint = get_delivery_base_url()

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
end
