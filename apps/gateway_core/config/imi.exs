use Mix.Config

config :gateway_core, Red9Cobra.IMI,
  nodes: [:"n2@s1.kafka.local"],
  max_charge_retries: 10,
  http_identifier_header: System.get_env("IMI_HTTP_ID_HEADER") || "X-RED9COBRA-ID",
  name: "IMI",
  throttle: [{2, 5_000}, {10, 60_000}],
  max_charge_amount_per_day_user: 500,
  max_messages_per_sec: 60,
  max_messages_per_day_user: 10,
  max_total_charge_per_month: 10_000,
  membership_message_cron: "* 9 * * 2",
  # sms_center_host: ["172.17.251.18", "172.17.251.19", "172.17.251.20", "172.17.251.21"],
  # client_host: "10.20.197.211",
  # Q Config
  input_Q: System.get_env("IMI_IN_Q") || "imi_input_q",
  success_Q: System.get_env("IMI_SUCCESS_Q") || "imi_success_q",
  fail_Q: System.get_env("IMI_FAIL_Q") || "imi_fail_q",
  # SMS CENTER
  sms_center_host: (System.get_env("IMI_SMS_CENTER_HOSTS") || "imi.red9.ir") |> String.split(","),
  sms_center_port: System.get_env("IMI_SMS_CENTER_PORT") || "443",
  sms_center_request_scheme: System.get_env("IMI_SMS_CENTER_SCHEME") || "https",

  # Basic cmVkOTpBODg5OWdoanRyZQ==
  sms_center_auth: System.get_env("IMI_SMS_CENTER_AUTH") || "Basic cmVkOTpBODg5OWdoanRyZQ==",

  # OTP
  otp_center_host: (System.get_env("IMI_OTP_CENTER_HOSTS") || "imi.red9.ir") |> String.split(","),
  otp_center_port: System.get_env("IMI_OTP_CENTER_PORT") || "443",
  otp_center_request_scheme: System.get_env("IMI_OTP_CENTER_SCHEME") || "https",
  otp_pushotp_url: System.get_env("IMI_OTP_CENTER_BASE_URL") || "/apigw/charging/pushotp",
  otp_chargeotp_url: System.get_env("IMI_OTP_CENTER_BASE_URL") || "/apigw/charging/chargeotp",

  # Basic cmVkOTpBODg5OWdoanRyZQ==
  otp_center_auth: System.get_env("IMI_OTP_CENTER_AUTH") || "Basic cmVkOTpBODg5OWdoanRyZQ==",

  # FTP TOOL
  ftp_endpoint: System.get_env("IMI_FTP_ENDPOINT"),

  # CODE51
  code51_endpoint: System.get_env("CODE51_ENDPOINT"),
  code51_token: System.get_env("CODE51_TOKEN"),

  # THIS MACHINE
  client_host: System.get_env("IMI_HOST_IP") || "127.0.0.1",
  client_port: System.get_env("IMI_HOST_PORT") || "8000",
  delivery_endpoint_scheme: System.get_env("IMI_HOST_SCHEME") || "http",
  devivery_address_template: "/delivery/<%= service_name %>/<%= method %>",
  http_timeout: System.get_env("IMI_WSDL_TIMEOUT"),
  wsdl_action_endpoint: "http://www.csapi.org/wsdl/parlayx",
  methods: %{
    send_sms_without_charge: %{
      timeout: 100_000,
      action: "/sms/send/v4_0/service/SendSms/sendSmsRequest",
      call_method: "parlayxsmsgw/services/SendSmsService/",
      template: """
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/send/v4_0/local">
      <soapenv:Header/>
      <soapenv:Body>
      <loc:sendSms>
      <loc:addresses><%= address %></loc:addresses>
      <loc:senderName><%= short_code %></loc:senderName>
      <loc:message><%= message %></loc:message>
      <loc:receiptRequest>
      <endpoint><%= delivery_endpoint %></endpoint> <interfaceName>RED9-Free-SMS</interfaceName> <correlator><%= correlator %></correlator>
      </loc:receiptRequest>
      </loc:sendSms>
      </soapenv:Body>
      </soapenv:Envelope>

      """
    },
    send_sms: %{
      action: "/sms/send/v4_0/service/SendSms/sendSmsRequest",
      call_method: "parlayxsmsgw/services/SendSmsService/",
      template: """
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/send/v4_0/local">
      <soapenv:Header/>
      <soapenv:Body>
      <loc:sendSms>
      <loc:addresses><%= address %></loc:addresses>
      <loc:senderName><%= short_code %></loc:senderName>
      <loc:charging>
      <description>DeliveryChannel=SMS|DiscoveryChannel=SMS|Origin=<%= short_code %>|ContentID=<%= correlator %></description><currency>RLS</currency>
      <amount>1</amount>
      <code><%= charge_code %></code>
      </loc:charging>
      <loc:message><%= message %></loc:message>
      </loc:sendSms>
      </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    single_charge: %{
      action: "/payment/AmountCharging/chargeAmountRequest",
      call_method: "parlayxchargeamountgw/services/AmountChargingService/",
      template: """
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/payment/amount_charging/v4_0/local">
      <soapenv:Header/>
      <soapenv:Body>
      <loc:chargeAmount>
      <loc:endUserIdentifier><%= address %></loc:endUserIdentifier>
      <loc:charge>
      <description>DeliveryChannel=SMS|DiscoveryChannel=SMS|origin=<%= short_code %>|ContentID=<%= correlator %></description>
      <currency>IRR</currency>
      <amount>1</amount>
      <code><%= charge_code %></code>
      </loc:charge>
      <loc:referenceCode><%= correlator %></loc:referenceCode>
      </loc:chargeAmount>
      </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    single_refund: %{
      action: "/payment/AmountCharging/chargeAmountRequest",
      call_method: "parlayxchargeamountgw/services/AmountChargingService/",
      template: """
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/payment/amount_charging/v4_0/local"> <soapenv:Header/>
      <soapenv:Body>
      <loc:refundAmount>
      <loc:endUserIdentifier><%= address %></loc:endUserIdentifier>
      <loc:charge> <description>Refund|refundTid=<%= refund_ticket_id %></description> <code><%= charge_code %></code>
      </loc:charge> <loc:referenceCode><%= correlator %></loc:referenceCode> </loc:refundAmount>
      </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    bulk_sms: %{
      action: "",
      call_method: "parlayxsmsgwbulk/services/SendSmsServiceBulk/",
      template: """
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/send/v4_0/local">
      <soapenv:Header></soapenv:Header>
      <soapenv:Body>
      <loc:sendSms>
      <%= for message <- messages do %>
      <% {contact, m} = message %>
      <loc:sendmessage>
      		<addresses><%= contact.area_code %><%= contact.national_number %></addresses>
      		<senderName><%= short_code %></senderName>
      		<message><%= m.message %></message>
      </loc:sendmessage>
      <% end %>
      </loc:sendSms>
      </soapenv:Body>
      </soapenv:Envelope>
      """
    }
  },
  gouping_panel: %{
    username: "MCIGP",
    password: "MciGP9xfgii",
    host: "https://api.appido.ir/gpanel",
    port: 443
  }
