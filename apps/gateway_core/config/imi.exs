use Mix.Config

config :red9cobra, Red9Cobra.IMI,
  max_charge_retries: 10,
  name: "IMI",
  max_charge_amount_per_day_user: 500,
  max_messages_per_sec: 60,
  max_messages_per_day_user: 10,
  max_total_charge_per_month: 10_000,
  membership_message_cron: "* 9 * * 2",
  # sms_center_host: ["172.17.251.18", "172.17.251.19", "172.17.251.20", "172.17.251.21"],
  # client_host: "10.20.197.211",

  # SMS CENTER
  sms_center_host: (System.get_env("IMI_SMS_CENTER_HOSTS") || "localhost") |> String.split(","),
  sms_center_port: System.get_env("IMI_SMS_CENTER_PORT") || "9000",
  sms_center_request_scheme: System.get_env("IMI_SMS_CENTER_SCHEME") || "http",

  # Basic cmVkOTpBODg5OWdoanRyZQ==
  sms_center_auth: System.get_env("IMI_SMS_CENTER_AUTH"),
  # FTP TOOL
  ftp_endpoint: System.get_env("IMI_FTP_ENDPOINT"),

  # CODE51
  code51_endpoint: System.get_env("CODE51_ENDPOINT"),
  code51_token: System.get_env("CODE51_TOKEN"),

  # THIS MACHINE
  client_host: System.get_env("IMI_HOST_IP"),
  client_port: System.get_env("IMI_HOST_PORT"),
  delivery_endpoint_scheme: System.get_env("IMI_HOST_SCHEME"),
  devivery_address_template: "/delivery/<%= service_name %>/<%= method %>",
  wsdl_action_endpoint: "http://www.csapi.org/wsdl/parlayx",
  methods: %{
    send_sms_without_charge: %{
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
