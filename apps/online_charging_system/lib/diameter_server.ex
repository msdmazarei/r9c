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




  def tcp_cap() do
    {:diameter_service, self(),
     {:diameter_caps, <<"diaserver.msd">>, <<"diaserver.msd">>, [], 3_285_213, <<"RCobra OCS">>,
      [], [50386, 13], [1, 4], [], [0, 167_772_151, 1, 4], [], [1], []},
     [
       {:diameter_app, :base, :diameter_gen_base_rfc6733,
        ['Elixir.OnlineChargingSystem.Servers.DiameterCallbackApp'], %{}, 0, false,
        [{:answer_errors, :discard}, {:request_errors, :answer_3xxx}]},
       {:diameter_app, :nas_application, :diameter_gen_nas_application_rfc7155,
        ['Elixir.OnlineChargingSystem.Servers.DiameterCallbackApp'], %{}, 1, false,
        [{:answer_errors, :discard}, {:request_errors, :answer_3xxx}]},
       {:diameter_app, :ro_3gpp, :diameter_gen_3gpp_ro_application,
        ['Elixir.OnlineChargingSystem.Servers.DiameterCallbackApp'], %{}, 4, false,
        [{:answer_errors, :discard}, {:request_errors, :answer_3xxx}]},
       {:diameter_app, :cc, :diameter_gen_msd_application,
        ['Elixir.OnlineChargingSystem.Servers.DiameterCallbackApp'], %{}, 167_772_151, false,
        [
          {:answer_errors, :discard},
          {:request_errors, :answer_3xxx}
        ]}
     ]}
  end

  def w8_message() do
    Logging.debug("waiting for message")

    receive do
      x ->
        Logging.debug("messge arrgivedx:~p", [x])
        # code
    end

    w8_message()
  end

  def only_start_transport_tcp() do
    Process.spawn(
      fn ->
        { :ok, r} = :diameter.add_transport(
          :nas_application,
          {:listen,
           [
             {:transport_module, :diameter_tcp},
             {:transport_config, [{:reuseaddr, true}, {:ip, {127, 0, 0, 1}}, {:port, 3868}]}
           ]}
        )

        res =
          :diameter_tcp.start(
            {:accept, r},
            tcp_cap(),
            [{:reuseaddr, true}, {:ip, {127, 0, 0, 1}}, {:port, 3868}]
          )

        Logging.debug("resilt of tcp: ~p", [res])
        w8_message()
      end,
      []
    )
  end

end
