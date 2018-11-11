defmodule GatewayCore.Outputs.IMI do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  use GatewayCore.Outputs.Red9CobraSimpleOutGW
  # require IEx
  @gateway_config Application.get_env(:gateway_core, Red9Cobra.IMI)
  @wsdl_action_endpoint @gateway_config[:wsdl_action_endpoint]
  @basic_auth @gateway_config[:sms_center_auth]

  @spec send_sms_to_gw(%DatabaseEngine.Models.SMS{}) :: :ok
  def send_sms_to_gw(
        sms_to_send = %DatabaseEngine.Models.SMS{
          options: %{:imi_service_key => service_key, :imi_sms_type => sms_type}
        }
      ) do
    timeout = get_timeout(sms_type)
    {:ok, body_to_send, action, _} = get_send_sms_post_data(sms_to_send)
    url = get_call_url(sms_type)

    basic_header =
      case @basic_auth do
        nil -> nil
        _ -> Utilities.HTTP1_1.authotization_header(@basic_auth)
      end

    service_key_header = Utilities.HTTP1_1.header("Servicekey", service_key)

    headers =
      case basic_header do
        nil ->
          [service_key_header]

        _ ->
          [basic_header, service_key_header]
      end

    {:ok, status_code, _, _} = Utilities.HTTP1_1.wsdl(url, action, headers, body_to_send, timeout)

    if status_code > 199 && status_code < 300 do
      Logging.info("message_id:~p sent successfully", [sms_to_send.id])
    else
      Logging.warn("sms.id:~p sent failed, status_code:~p", [sms_to_send.id, status_code])
    end

    :ok
  end

  def gw_init() do
  end

  def gw_queue_list() do
  end

  def send_sms_list(_, _) do
  end

  @spec get_sms_center_base_url() :: String.t()
  defp get_sms_center_base_url() do
    Logging.debug("Called with config: ~p", [@gateway_config])

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
    action = (@wsdl_action_endpoint || "") <> (send_sms_config[:action] || "")

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
