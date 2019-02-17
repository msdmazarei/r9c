defmodule OnlineChargingSystem.Servers.DiameterServer do
  @moduledoc false
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def start_server() do
    port = 3868
    svc_name = :base

    service_options = [
      {
        :application,
        [
          {:alias, :base},
          {:dictionary, :diameter_gen_base_rfc6733},
          {:module, OnlineChargingSystem.Servers.DiameterCallbackApp},
          {:state, %{}}
        ]
      },
      {
        :application,
        [
          {:alias, :ro_3gpp},
          {:dictionary, :diameter_gen_3gpp_ro_application},
          {:module, OnlineChargingSystem.Servers.DiameterCallbackApp},
          {:state, %{}}
        ]
      },
      {
        :application,
        [
          {:alias, :cc},
          {:dictionary, :diameter_gen_msd_application},
          {:module, OnlineChargingSystem.Servers.DiameterCallbackApp},
          {:state, %{}}
        ]
      },
      {:restrict_connections, false},
      {:string_decode, false},
      {:"Origin-Host", "diaserver.msd"},
      {:"Origin-Realm", "diaserver.msd"},
      {:"Vendor-Id", 3_285_213},
      {:"Product-Name", "RCobra OCS"},
      {:"Supported-Vendor-Id", [50386, 13]},
      {:"Firmware-Revision", 1},
      {:"Auth-Application-Id", [4]},
      {:"Acct-Application-Id", [0, 167_772_151]}
    ]

    Logging.debug("svc_name:~p service_options:~p", [svc_name, service_options])
    :ok = :diameter.start_service(svc_name, service_options)

    t_options = [
      {:transport_module, :diameter_tcp},
      {
        :transport_config,
        [
          {:reuseaddr, true},
          {:ip, {127, 0, 0, 1}},
          {:port, port}
        ]
      }
    ]

    :diameter.add_transport(svc_name, {:listen, t_options})
  end
end
