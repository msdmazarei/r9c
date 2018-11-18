defmodule GatewayCore.Outputs.IMI.OTP do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  # require IEx
  @gateway_config Application.get_env(:gateway_core, Red9Cobra.IMI)
  @otp_basic_auth @gateway_config[:otp_center_auth]
  @compile {:inline, get_otp_url: 1}
  @compile {:inline, process_otp_api: 3}
  @compile {:inline, process_otp_api_response: 3}
  @compile {:inline, callback: 2}

  @spec otp_req_to_gw(%DatabaseEngine.Models.OTP.VAS{}) :: boolean
  def otp_req_to_gw(
        vas_otp = %DatabaseEngine.Models.OTP.VAS{
          :gw_option => %DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS{},
          :options => %{
            "max_daily_allowed_charge" => max_daily_allowed_charge,
            "service_name_fa" => service_name_fa
          },
          :otp_type => "push"
        }
      ) do
    Logging.debug("Called. Params:~p", [vas_otp])
    sk = vas_otp.gw_option.service_key
    sn = service_name_fa || vas_otp.service_name
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
        "amount" => max_daily_allowed_charge |> to_string |> String.to_integer(),
        "description" => "DESC: Subscription to #{sn} service"
      }
    }

    Logging.debug("Data to Send To Server:~p", [data])
    url = get_otp_url(:otp_pushotp_url)
    Logging.debug("Url to Call:~p", [url])
    process_otp_api(vas_otp, url, data)
  end

  def otp_req_to_gw(
        vas_otp = %DatabaseEngine.Models.OTP.VAS{
          :gw_option => %DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS{},
          :options => %{
            "otp_transaction_id" => otp_transaction_id,
            "reference_code" => reference_code,
            "pin" => pin
          },
          :otp_type => otp_type
        }
      )
      when otp_type in ["confirm", "charge_confirm"] do
    sk = vas_otp.gw_option.service_key
    short_code = vas_otp.gw_option.short_code

    data = %{
      "accesInfo" => %{
        "servicekey" => sk,
        "msisdn" => vas_otp.contact,
        "otpTransactionId" => otp_transaction_id,
        "shortCode" => short_code,
        "transactionPIN" => "#{pin}",
        "referenceCode" => reference_code
      }
    }

    Logging.debug("Data to send to server:~p", [data])
    url = get_otp_url(:otp_chargeotp_url)
    Logging.debug("Url to Call:~p", [url])
    process_otp_api(vas_otp, url, data)
  end

  def otp_req_to_gw(
        vas_otp = %DatabaseEngine.Models.OTP.VAS{
          :gw_option => %DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS{
            :service_key => sk,
            :short_code => short_code
          },
          :service_name => service_name,
          :options => %{
            "service_name_fa" => service_name_fa,
            "reference_code" => reference_code,
            "charge_code" => charge_code,
            "charge_amount" => charge_amount,
            "reason_for_charge" => reason_for_charge
          },
          :otp_type => "charge",
          :contact => msisdn,
          :id => id
        }
      ) do
    sn = service_name_fa || service_name
    short_code = short_code |> to_string

    data = %{
      "accesInfo" => %{
        "servicekey" => sk,
        "msisdn" => msisdn,
        "serviceName" => sn,
        "referenceCode" => reference_code,
        "shortCode" => short_code,
        "contentId" => "RED9-C#{id}"
      },
      "charge" => %{
        "code" => charge_code,
        "amount" => charge_amount,
        "description" => "DESC: #{reason_for_charge}"
      }
    }

    Logging.debug("Data to send: ~p for id:~p", [data, id])
    url = get_otp_url(:otp_pushotp_url)
    Logging.debug("Url: ~p for id:~p", [url, id])
    process_otp_api(vas_otp, url, data)
  end

  def otp_req_to_gw(vas_otp) do
    Logging.error("Bad method call no gw_options found. vas_otp:~p", [vas_otp])
    false
  end

  defp process_otp_api_response(vas_otp, status_code, resp_body) do
    case status_code do
      200 ->
        {:ok, responseJson} = resp_body |> Jason.decode()

        case vas_otp.otp_type do
          "push" ->
            case responseJson["statusInfo"]["statusCode"] do
              "500" ->
                Logging.warn("OTP PUSH id:~p failed.", [vas_otp.id])

                %{
                  :success => false,
                  :error => responseJson["statusInfo"]["errorInfo"]
                }

              _ ->
                Logging.info("OTP PUSH id:~p done.", [vas_otp.id])

                %{
                  :success => true,
                  :otp_transaction_id => responseJson["statusInfo"]["OTPTransactionId"],
                  :reference_code => responseJson["statusInfo"]["referenceCode"],
                  :info => responseJson["statusInfo"]
                }
            end

          "confirm" ->
            case responseJson["statusInfo"]["statusCode"] do
              "200" ->
                Logging.info("Charged Successfully. id:~p", [vas_otp.id])
                %{:success => true}

              _ ->
                Logging.warn("Charged failed. id:~p,error:~p", [
                  vas_otp.id,
                  responseJson["statusInfo"]["errorInfo"]
                ])

                %{:success => false, :error => responseJson["statusInfo"]["errorInfo"]}
            end

          "charge" ->
            case responseJson["statusInfo"]["statusCode"] do
              "200" ->
                %{:success => true, :info => responseJson["statusInfo"]}

              _ ->
                %{:success => false, :error => responseJson["statusInfo"]["errorInfo"]}
            end
        end

      _ ->
        Logging.warn("OTP PUSH id:~p failed. status code:~p type:~p", [
          vas_otp.id,
          status_code,
          vas_otp.type
        ])

        %{
          :success => false
        }
    end
  end

  defp process_otp_api(
         vas_otp = %DatabaseEngine.Models.OTP.VAS{
           :internal_callback => internal_callback
         },
         url,
         data
       ) do
    headers =
      if @otp_basic_auth != nil do
        [Networking.HTTP1_1.authotization_header(@otp_basic_auth)]
      else
        []
      end

    {:ok, data_str} = data |> Jason.encode()

    {:ok, status_code, resp_headers, resp_body} =
      Networking.HTTP1_1.post(
        url,
        headers,
        "application/json",
        data_str,
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

    callback_data_api_response = process_otp_api_response(vas_otp, status_code, resp_body)
    callback_data = Map.merge(callback_data_http, callback_data_api_response)
    Logging.debug("callback_data:~p", [callback_data])
    callback(callback_data, internal_callback)
    callback_data[:success]
  end

  @spec get_otp_url(:otp_chargeotp_url | :otp_pushotp_url) :: String.t()
  defp get_otp_url(type) do
    @gateway_config[:otp_center_request_scheme] <>
      "://" <>
      (@gateway_config[:otp_center_host]
       |> Enum.random()) <> ":" <> @gateway_config[:otp_center_port] <> @gateway_config[type]
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
end
