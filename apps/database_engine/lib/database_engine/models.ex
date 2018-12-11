defmodule DatabaseEngine.Models.InternalCallback do
  defstruct module_name: "",
            function_name: "",
            arguments: []
end

defmodule DatabaseEngine.Models.SMS do
  @moduledoc false
  defstruct sender: nil,
            receiver: nil,
            sms_center: nil,
            sent_epoch: 0,
            received_epoch: 0,
            body: nil,
            id: nil,
            # options could contain below:
            #
            # for imi gateway:
            #    imi_charge_code: charge code
            #    imi_short_code: short code
            #    imi_service_key : service key
            #    imi_sms_type : :send_sms|:send_sms_without_charge
            # for irmtn gateway:
            #    "irmtn_short_code": short code
            #    "irmtn_linkid": link id
            #    "irmtn_service_id" : service id
            #    "irmtn_spid": spid
            #    "irmtn_sms_type" : "send_sms"
            # could contains "reply_to_message" key which specifies
            # the original message that current message is replied
            # to it
            # could contain "gateway" which specify gateway like: "DUMMY","IMI","IRMTN"
            # could contain __module_name
            # could contain __state_desc
            # could contain __state_time
            options: %{},
            internal_callback: nil
end

defmodule DatabaseEngine.Models.SMS.Helper do
  def describe_stage(sms, module_name, state_descrition) do
    o = sms.options || %{}

    o =
      o
      |> Map.put("__module_name", module_name)
      |> Map.put("__state_desc", state_descrition)
      |> Map.put("__state_time", Utilities.now())

    %DatabaseEngine.Models.SMS{sms | options: o}
  end
end

defmodule DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS do
  defstruct service_key: nil,
            subscriber_code: nil,
            short_code: nil
end

defmodule DatabaseEngine.Models.Charge.VAS do
  defstruct contact: nil,
            service_name: nil,
            service_id: nil,
            # could contain "spid" for irmtn
            options: %{},
            internal_callback: nil,
            id: nil,
            charge_amount: nil
end

defmodule DatabaseEngine.Models.OTP.VAS do
  defstruct contact: nil,
            service_name: nil,
            service_id: nil,
            id: nil,
            # could contain "max_daily_allowed_charge" for imi in push mode
            # could contain "pin","otp_transaction_id" , "reference_code" for imi in confirm mode
            # could contain service_name_fa
            # could contan "reference_codereference_code" for imi in charge mode
            # could contain "charge_code" for imi in "charge" mode
            # could contain "charge_amount" for imi in "charge" mode
            # could contain "reason_for_charge" for imi in "charge" mode
            options: %{},
            internal_callback: nil,
            # could be DatabaseEngine.Models.OTP.VAS.IMI_GW
            gw_option: nil,
            # can be "push" or "confirm" or "charge" or "charge_confirm"
            otp_type: nil
end

defimpl Jason.Encoder,
  for: [
    DatabaseEngine.Models.SMS,
    DatabaseEngine.Models.OTP.VAS,
    DatabaseEngine.Models.OTP.VAS.IMI_GW_OPTS,
    DatabaseEngine.Models.InternalCallback,
    DatabaseEngine.Models.Charge.VAS
  ] do
  def encode(struct, opts) do
    m = Map.from_struct(struct)
    m = Map.put(m, :__orig_struct__, struct.__struct__)
    Jason.Encode.map(m, opts)
  end
end

defimpl DatabaseEngine.DurableQueue.Deserialize,
  for: [Map] do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def deserialize(data) do
    data =
      if is_map(data) do
        new_data =
          Enum.reduce(Map.to_list(data), %{}, fn {k, v}, acc ->
            new_v =
              if is_map(v) and v["__orig_struct__"] != nil do
                DatabaseEngine.DurableQueue.Deserialize.deserialize(v)
              else
                v
              end

            Map.put(acc, k, new_v)
          end)

        new_data
      else
        data
      end

    Logging.debug("data:~p", [data])

    struct_name = String.to_atom(data["__orig_struct__"])
    result = Utilities.to_struct(struct_name, data)

    case struct_name do
      DatabaseEngine.Models.SMS ->
        f = fn key, val ->
          if ["imi_charge_code", "imi_short_code", "imi_service_key", "imi_sms_type"]
             |> Enum.member?(key) do
            {String.to_atom(key), val}
          else
            {key, val}
          end
        end

        correct_options = for {key, val} <- result.options, into: %{}, do: f.(key, val)

        correct_options =
          case correct_options[:imi_sms_type] do
            nil ->
              correct_options

            _ ->
              Map.put(
                correct_options,
                :imi_sms_type,
                String.to_atom(correct_options[:imi_sms_type])
              )
          end

        Logging.debug("options:~p", [correct_options])

        %DatabaseEngine.Models.SMS{result | options: correct_options}

      _ ->
        result
    end
  end
end

defmodule DatabaseEngine.Struct.AuthUser do
  @moduledoc """
    every user will have this struct
  """
  @derive Jason.Encoder
  @enforce_keys [:key, :name, :company, :contact_number, :email, :domain]
  defstruct key: nil,
            name: nil,
            idx: nil,
            company: nil,
            contact_number: nil,
            email: nil,
            domain: nil
end

defmodule DatabaseEngine.Struct.Service.VAS do
  @moduledoc """
    This struct contains options of a service as defined in
    mnesia schema.
  """

  @derive Jason.Encoder
  @enforce_keys [:shortcode]
  defstruct uuid: UUID.uuid4(),
            shortcode: nil,
            gateway: nil,
            is_commercial: true,
            alternative_shortcode: nil,
            name: nil,
            fa_name: nil,
            daily_price: 0,
            has_free_trial: false,
            free_trial_period: 0,
            sid: 0,
            has_app: false,
            is_wappush: false,
            apk_link: nil,
            ios_link: nil,
            has_website: false,
            website_link: nil,
            sub_url: nil,
            unsub_url: nil,
            aggregator: nil,
            sdp: nil,
            signed_brand_obligation_letter: false,
            service_type: -1,
            renewal_url: nil,
            mo_url: nil,
            delivery_url: nil,
            service_key: nil,
            mci_contact_point: nil,
            mci_technical_contact_point: nil,
            sp_contact_point: nil,
            sp_technical_contact_point: nil,
            cp_contact_point: nil,
            cp_technical_contact_point: nil,
            white_list_msisdns: [],
            latest_test_result: nil,
            latest_test_datetime: nil,
            welcome_template: nil,
            junk_template: nil,
            wrong_template: nil,
            empty_template: nil,
            fa_activation_keys: ["۱"],
            en_activation_keys: ["1"],
            fa_deactivation_keys: ["خاموش"],
            en_deactivation_keys: ["off"]
end

defmodule DatabaseEngine.Struct.TableInternalData do
  @enforce_keys [:unixtime]
  defstruct unixtime: nil,
            modifier: nil,
            last_modification_unixtime: nil
end

defmodule DatabaseEngine.Struct.Gateway do
  @derive Jason.Encoder
  @enforce_keys []
  defstruct whitelist: [],
            blacklist: []
end

defmodule DatabaseEngine.Struct.NetACL do
  @enforce_keys []
  defstruct whitelist_cidrs: [],
            blacklist_cidrs: []
end

defmodule DatabaseEngine.Struct.App.VAS do
  @derive Jason.Encoder
  @enforce_keys []
  defstruct mo_callback_url: nil,
            subscription_callback_url: nil,
            unsubscription_callback_url: nil,
            renewal_callback_url: nil,
            delivery_callback_url: nil,
            general_callback_url: nil,
            net_acl: %DatabaseEngine.Struct.NetACL{}
end

defmodule DatabaseEngine.Struct.Apikey do
  @derive Jason.Encoder
  @enforce_keys []
  defstruct net_acl: %DatabaseEngine.Struct.NetACL{}
end
