defmodule GatewayCore.Utils do
  @moduledoc false

  def callback(_, nil) do
    :ok
  end

  def callback(
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

defmodule GatewayCore.Utils.VAS.GW do
  @compile {:inline, template: 2}
  @compile {:inline, action: 2}
  @compile {:inline, get_sms_center_base_url: 1}
  @compile {:inline, get_call_url: 2}

  def template(gateway_config,type) do
      gateway_config[:methods][type][:template]

  end
  def action(gateway_config,type) do
    gateway_config[:wsdl_action_endpoint] <> gateway_config[:methods][type][:action]
  end



  def get_sms_center_base_url(gateway_config) do
    gateway_config[:sms_center_request_scheme] <>
      "://" <>
      (gateway_config[:sms_center_host] |> Enum.random()) <>
      ":" <> gateway_config[:sms_center_port]
  end

  def get_call_url(gateway_config, type) do
    get_sms_center_base_url(gateway_config) <> "/" <> gateway_config[:methods][type][:call_method]
  end

  def get_request_headers(gateway_config, id, basic_auth, others \\ []) do
    basic_header =
      case basic_auth do
        nil -> nil
        _ -> Networking.HTTP1_1.authotization_header(basic_auth)
      end

    identifier_header =
      case gateway_config[:http_identifier_header] do
        nil -> nil
        v -> Networking.HTTP1_1.header(v, id)
      end

    headers = [basic_header, identifier_header] ++ others
    headers |> Enum.filter(fn x -> x != nil end)
  end

  def get_timeout(gateway_config, type) do
    gateway_config[:methods][type][:timeout] || gateway_config[:http_timeout]
  end

  def irmtn_process_failed_response(resp_body, error_codes, callback_data) do
    doc = resp_body |> Exml.parse()

    code =
      case Exml.get(doc, "//variables") do
        [_sn, err] ->
          err |> String.to_integer()

        err ->
          err |> String.to_integer()
      end

    fault = Exml.get(doc, "//text")
    fault_code = Exml.get(doc, "//faultcode")

    reason = %{
      fault: fault,
      fault_code: fault_code,
      code: code,
      description: Map.get(error_codes, code),
      action:
        case code do
          3312 ->
            :do_not_charge

          3101 ->
            :retry_again

          107 ->
            :fix_request

          _ ->
            nil
        end
    }

    {Map.merge(callback_data, %{:error => reason}), false}
  end
end
