defmodule MnesiaTablesTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias DatabaseEngine.Mnesia.DbSetup, as: DbSetup
  alias DatabaseEngine.Client, as: Client

  setup_all do
    Application.put_env(:mnesia, :dir, '/tmp/dbengine_unittests_#{UUID.uuid4()}')

    :ok
  end

  describe "Client Table" do
    setup do
      :ok
    end

    test "creating a client" do
      assert {:atomic, :ok} ==
               Client.new(%Client{
                 company: "rashavas",
                 email: "ashouri@rashavas.com",
                 name: "farsheed ashouri",
                 cellphone: "+989120228207",
                 acl: %{
                   gateways: ["imi", "mtn"],
                   service_types: ["2g", "3g", "wappush"]
                 },
                 limitations: %{
                   max_gateway_count: -1,
                   max_service_count: 2,
                   max_app_count: 10,
                   max_api_count: 20,
                   max_sms_tpm: 1200,
                   max_mms_tpm: 1200,
                   max_charge_tpm: 1200,
                   max_otp_tpm: 1200,
                   max_email_tpm: 1200,
                   max_push_tpm: 1200,
                   ip_whitelist: ["*"]
                 },
                 report_emails: ["rodmena@me.com"],
                 ## per day,
                 report_frequency: 1
               })

      cl = Client.get("ashouri@rashavas.com")
      assert cl.name == "Farsheed Ashorui"
      assert cl.limitations.max_sms_tpm == 1200
      assert "imi" in cl.acl.gateways
    end
  end
end
