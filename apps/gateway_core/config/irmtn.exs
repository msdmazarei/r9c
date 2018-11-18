use Mix.Config

config :gateway_core, Red9Cobra.IRMTN,
  http_identifier_header: System.get_env("IRMTN_HTTP_ID_HEADER") || "X-RED9COBRA-ID",
  name: "IRANCELL",
  max_charge_amount_per_day_user: 500,
  max_messages_per_sec: 60,
  max_messages_per_day_user: 10,
  error_codes: %{
    -1 => "Operation failed",
    0 => "Operation succeeded",
    100 => "The user does not exist",
    119 => "bill format error",
    120 => "This type of service does not exist",
    121 => "reception error",
    122 => "The service price is negative",
    123 => "service price format error",
    124 => "The service price exceeds the range",
    125 => "Subscriber brand is invalid",
    126 => "This subscriber does not have sufficient balance",
    127 => "Recharge failed",
    1000 => "Charging message parameter is wrong",
    1001 => "no response to charging information",
    1002 => "Subscriber status is invalid",
    1003 => "Bill generation failed",
    1004 => "Prepaid charging account is invalid",
    1005 => "SCP operation failed",
    1006 => "overlapped bill",
    1007 => "Saving bill failed",
    1008 => "premium rate bill",
    1009 => "Charging preserved conversation timed out",
    1010 => "Canceling subscription failed",
    1011 => "Rating authentication failed",
    1012 => "Waiting confirmation message timed out",
    1013 => "Waiting real-time response timed out",
    1014 => "Other timed out",
    1999 => "Operation failed",
    2010 => "Subscriber status is unavailable",
    6031 => "Charge confirmation failed",
    6042 => "Balance refund failed",
    6504 => "additional MT limit",
    0 => "Operation succeeded",
    2 => "The entered password is different from the registered one",
    7 => "The session times out",
    8 => "Incorrect protocol version number",
    100 => "Invalid field",
    107 => "Incorrect MSISDN",
    502 => "Abnormal network",
    505 => "Authentication of the carrier or the user failed",
    507 => "TPS exceeds the threshold limit",
    1105 => "abnormal user status",
    3101 => "insufficient balance",
    3312 => "Subscription does not exist",
    3323 => "Other errors returned by the IN",
    3335 => "limited additional MT"
  },
  max_total_charge_per_month: 10_000,
  max_user_charge_per_day: 300,
  max_user_charge_retry_count: 4,
  membership_message_cron: "* 9 * * 2",
  sms_center_request_scheme: "https",
  delivery_endpoint_scheme: "http",
  sms_center_host: ["irancell.red9.ir"],
  sms_center_auth: "Basic cmVkOTpBODg5OWdoanRyZQ==",
  client_host: "37.228.136.110",
  sms_center_port: "443",
  devivery_address_template: "/delivery/irancell/<%= service_name %>/<%= method %>",
  client_port: "4050",
  wsdl_action_endpoint: "",
  methods: %{
    receive_sms: %{
      action: "",
      call_method: "ReceiveSmsService/services/ReceiveSms",
      template: """
      <?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v2="http://www.huawei.com.cn/schema/common/v2_1" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/receive/v2_2/local">
         <soapenv:Header>
                <v2:RequestSOAPHeader>
                       <v2:spId><%= spid %></v2:spId>
                       <v2:serviceId><%= service_id %></v2:serviceId>
                   </v2:RequestSOAPHeader>
            </soapenv:Header>
         <soapenv:Body>
                <loc:getReceivedSms>
                       <loc:registrationIdentifier><%= short_code %></loc:registrationIdentifier>
                   </loc:getReceivedSms>
            </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    send_sms: %{
      action: "",
      call_method: "SendSmsService/services/SendSms",
      template: """
      <?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v2="http://www.huawei.com.cn/schema/common/v2_1" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/send/v2_2/local">
         <soapenv:Header>
                <v2:RequestSOAPHeader>
                       <v2:spId><%= spid %></v2:spId>
                       <v2:serviceId><%= service_id %></v2:serviceId>
                       <%= if linkid do %>
                          <v2:linkid><%= linkid %></v2:linkid>
                       <% end %>
                   </v2:RequestSOAPHeader>
            </soapenv:Header>
         <soapenv:Body>
                <loc:sendSms>
                       <loc:addresses>tel:<%= address %></loc:addresses>
                       <loc:senderName><%= short_code %></loc:senderName>
                       <loc:message><%= message %></loc:message>
                       <loc:receiptRequest>
                              <endpoint>http://37.228.136.110:4050</endpoint>
                              <interfaceName>SmsNotification</interfaceName>
                              <correlator><%= correlator |> String.replace("-", "_") %></correlator>
                          </loc:receiptRequest>
                   </loc:sendSms>
            </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    charge: %{
      action: "",
      call_method: "AmountChargingService/services/AmountCharging",
      template: """
      <?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:loc="http://www.csapi.org/schema/parlayx/payment/amount_charging/v2_1/local">
      <soapenv:Header>
      <RequestSOAPHeader xmlns="http://www.huawei.com.cn/schema/common/v2_1">
      <spId><%= spid %></spId> <serviceId><%= service_id %></serviceId> <OA><%= address %></OA>
      <FA><%= address %></FA>
        </RequestSOAPHeader>
      </soapenv:Header>
      <soapenv:Body>
        <loc:chargeAmount>

      <loc:endUserIdentifier><%= address %></loc:endUserIdentifier> <loc:charge>
             <description>charge</description>
             <currency>IRR</currency>
             <amount><%= charge_amount %></amount>
             <code>4</code>
           </loc:charge>
          <loc:referenceCode><%= correlator |> String.replace("-", "_") %></loc:referenceCode> </loc:chargeAmount>
        </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    start_notification: %{
      action: "",
      call_method: "SmsNotificationManagerService/services/SmsNotificationManager",
      template: """
      <?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v2="http://www.huawei.com.cn/schema/common/v2_1" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/notification_manager/v2_3/local">
       <soapenv:Header>
              <RequestSOAPHeader xmlns="http://www.huawei.com.cn/schema/common/v2_1">
      <spId><%= spid %></spId>
      <serviceId><%= service_id %></serviceId>
                 </RequestSOAPHeader>
          </soapenv:Header>
       <soapenv:Body>
              <loc:startSmsNotification>
                     <loc:reference>
                            <endpoint>http://37.228.136.110:4050</endpoint>
                            <interfaceName>notifySmsReception</interfaceName>
                            <correlator>RED9_<%= service_id %></correlator>
                            <criteria></criteria>
                        </loc:reference>
      <loc:smsServiceActivationNumber><%= short_code %></loc:smsServiceActivationNumber>
                 </loc:startSmsNotification>
          </soapenv:Body>
      </soapenv:Envelope>
      """
    },
    stop_notification: %{
      action: "",
      call_method: "SmsNotificationManagerService/services/SmsNotificationManager",
      template: """
      <?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v2="http://www.huawei.com.cn/schema/common/v2_1" xmlns:loc="http://www.csapi.org/schema/parlayx/sms/notification_manager/v2_3/local">
       <soapenv:Header>
              <v2:RequestSOAPHeader>
                     <v2:spId><%= spid %></v2:spId>
                     <v2:serviceId><%= service_id %></v2:serviceId>
                 </v2:RequestSOAPHeader>
          </soapenv:Header>
       <soapenv:Body>
              <loc:stopSmsNotification>
                     <loc:correlator>RED9_<%= service_id %></loc:correlator>
                     <loc:criteria></loc:criteria>
                 </loc:stopSmsNotification>
          </soapenv:Body>
      </soapenv:Envelope>
      """
    }
  },
  gouping_panel: %{}
