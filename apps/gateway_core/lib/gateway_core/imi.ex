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
  @otp_basic_auth @gateway_config[:otp_center_auth]
  @compile {:inline, get_pushotp_url: 0}

  @spec otp_push_req_to_gw(%DatabaseEngine.Models.OTP.VAS{}) :: boolean
  def otp_push_req_to_gw(
        vas_otp = %DatabaseEngine.Models.OTP.VAS{
          :gw_option => %DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS{},
          :options => %{:max_daily_allowed_charge => max_daily_allowed_charge},
          :internal_callback => internal_callback
        }
      ) do
    Logging.debug("Called. Params:~p", [vas_otp])
    sk = vas_otp.gw_option.service_key
    sn = vas_otp.gw_option.service_name_fa || vas_otp.service_name
    sc = vas_otp.gw_option.subscriber_code
    short_code = vas_otp.gw_option.short_code

    data = %{
      "accesInfo" => %{
        "servicekey" => sk,
        "msisdn" => vas_otp.contact,
        "serviceName" => sn,
        "referenceCode" => vas_otp.id,
        "shortCode" => short_code,
        "contentId" => "RED9-S-#{vas_otp.id}"
      },
      "charge" => %{
        "code" => sc,
        "amount" => (max_daily_allowed_charge |> to_string |> String.to_integer()) * 10,
        "description" => "DESC: Subscription to #{sn} service"
      }
    }

    Logging.debug("Data to Send To Server:~p", [data])
    url = get_pushotp_url()
    Logging.debug("Url to Call:~p", [url])

    headers =
      if @otp_basic_auth != nil do
        Networking.HTTP1_1.authotization_header(@otp_basic_auth)
      else
        []
      end

    {:ok, status_code, resp_headers, resp_body} =
      Networking.HTTP1_1.post(
        url,
        headers,
        "application/json",
        data |> Jason.encode(),
        nil,
        vas_otp.id
      )

    callback_data_http = %{
      :http => %{
        :status_code => status_code,
        :headers => resp_headers,
        :body => resp_body
      }
    }

    callback_data =
      case status_code do
        200 ->
          {:ok, responseJson} = resp_body |> Jason.decode()

          case responseJson["statusInfo"]["statusCode"] do
            "500" ->
              Logging.warn("OTP PUSH id:~p failed.", [vas_otp.id])

              Map.merge(
                %{
                  :success => false,
                  :error => responseJson["statusInfo"]["errorInfo"]
                },
                callback_data_http
              )

            _ ->
              Logging.info("OTP PUSH id:~p done.", [vas_otp.id])

              Map.merge(callback_data_http, %{
                :success => true,
                :info => responseJson["statusInfo"]
              })
          end

        _ ->
          Logging.warn("OTP PUSH id:~p failed. status code:~p", [vas_otp.id, status_code])

          %{
            :http => %{
              :status_code => status_code,
              :headers => resp_headers,
              :body => resp_body
            },
            :success => false
          }
      end

    callback(callback_data, internal_callback)
    callback_data[:success]
  end

  def otp_push_req_to_gw(vas_otp) do
    Logging.error("Bad method call no gw_options found. vas_otp:~p", [vas_otp])
    false
  end

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
    results = sms_list_to_send |> Enum.map(&send_sms_to_gw/1)
    r = {state, results}
    Logging.debug("Retuens:~p", [r])
    r
  end

  def send_otp_list(otp_list, state) do
    Logging.debug("Called")
    results = otp_list |> Enum.map(fn x -> true end)
    r = {state, results}
    Logging.debug("Returns:~p", [r])
    r
  end

  defp callback(_, nil) do
    :ok
  end

  defp callback(
         data,
         internal_callback = %DatabaseEngine.Models.InternalCallback{
           :module_name => module,
           :function_name => function,
           :arguments => arguments
         }
       ) do
    try do
      module =
        if is_atom(module) == false do
          String.to_atom(module)
        end

      function =
        if is_atom(function) == false do
          String.to_atom(function)
        end

      arguments = arguments ++ [data]
      Kernel.apply(module, function, arguments)
    rescue
      e ->
        Logging.error("problem to call internal_callback:~p error:~p", [internal_callback, e])
    end
  end

  @spec get_pushotp_url() :: String.t()
  defp get_pushotp_url() do
    @gateway_config[:otp_center_request_scheme] <>
      "://" <>
      (@gateway_config[:otp_center_host]
       |> Enum.random()) <>
      ":" <> @gateway_config[:otp_center_port] <> @gateway_config[:otp_pushotp_url]
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
