defmodule DatabaseEngine.Models do
  @moduledoc false

  defmodule SMS do
    @derive Jason.Encoder
    defstruct sender: "",
              receiver: "",
              sms_center: "",
              sent_epoch: 0,
              received_epoch: 0,
              body: ""
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
