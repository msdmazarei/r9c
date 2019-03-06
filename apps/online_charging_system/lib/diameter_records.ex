defmodule OnlineChargingSystem.Records.Diameter.Definitions do
  require Record

  Record.defrecord(
    :diameter_packet,
    Record.extract(:diameter_packet, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_event,
    Record.extract(:diameter_event, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_header,
    Record.extract(:diameter_header, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_avp,
    Record.extract(:diameter_avp, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_caps,
    Record.extract(:diameter_caps, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_uri,
    Record.extract(:diameter_uri, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_callback,
    Record.extract(:diameter_callback, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_service,
    Record.extract(:diameter_service, from_lib: "diameter/include/diameter.hrl")
  )

  Record.defrecord(
    :diameter_app,
    Record.extract(:diameter_app, from_lib: "diameter/include/diameter.hrl")
  )

  # [
  #   diameter_event: [service: :undefined, info: :undefined],
  #   diameter_packet: [
  #     header: :undefined,
  #     avps: :undefined,
  #     msg: :undefined,
  #     bin: :undefined,
  #     errors: [],
  #     transport_data: :undefined
  #   ],
  #   diameter_header: [
  #     version: :undefined,
  #     length: :undefined,
  #     cmd_code: :undefined,
  #     application_id: :undefined,
  #     hop_by_hop_id: :undefined,
  #     end_to_end_id: :undefined,
  #     is_request: :undefined,
  #     is_proxiable: :undefined,
  #     is_error: :undefined,
  #     is_retransmitted: :undefined
  #   ],
  #   diameter_avp: [
  #     code: :undefined,
  #     vendor_id: :undefined,
  #     is_mandatory: false,
  #     need_encryption: false,
  #     data: :undefined,
  #     name: :undefined,
  #     value: :undefined,
  #     type: :undefined,
  #     index: :undefined
  #   ],
  #   diameter_caps: [
  #     origin_host: :undefined,
  #     origin_realm: :undefined,
  #     host_ip_address: [],
  #     vendor_id: :undefined,
  #     product_name: :undefined,
  #     origin_state_id: [],
  #     supported_vendor_id: [],
  #     auth_application_id: [],
  #     inband_security_id: [],
  #     acct_application_id: [],
  #     vendor_specific_application_id: [],
  #     firmware_revision: [],
  #     avp: []
  #   ],
  #   diameter_uri: [
  #     type: :undefined,
  #     fqdn: :undefined,
  #     port: 3868,
  #     transport: :sctp,
  #     protocol: :diameter
  #   ],
  #   diameter_callback: [
  #     peer_up: :undefined,
  #     peer_down: :undefined,
  #     pick_peer: :undefined,
  #     prepare_request: :undefined,
  #     prepare_retransmit: :undefined,
  #     handle_request: :undefined,
  #     handle_answer: :undefined,
  #     handle_error: :undefined,
  #     default: :undefined,
  #     extra: []
  #   ],
  #   diameter_service: [
  #     pid: :undefined,
  #     capabilities: :undefined,
  #     applications: []
  #   ],
  #   diameter_app: [
  #     alias: :undefined,
  #     dictionary: :undefined,
  #     module: :undefined,
  #     init_state: :undefined,
  #     id: :undefined,
  #     mutable: false,
  #     options: [answer_errors: :discard, request_errors: :answer_3xxx]
  #   ]
  # ]
end

defmodule OnlineChargingSystem.Records.Diameter do
  require OnlineChargingSystem.Records.Diameter.Definitions
  require Record

  def record_to_map(x)
      when Record.is_record(x, :diameter_avp) do
    OnlineChargingSystem.Records.Diameter.Definitions.diameter_avp(x)
    |> Enum.into(%{})
  end

  def record_to_map(x)
      when Record.is_record(x, :diameter_header) do
    OnlineChargingSystem.Records.Diameter.Definitions.diameter_header(x)
    |> Enum.into(%{})
  end

  def record_to_map(x)
      when Record.is_record(x, :diameter_packet) do
    rtn =
      OnlineChargingSystem.Records.Diameter.Definitions.diameter_packet(x)
      |> Enum.into(%{})

    rtn =
      rtn
      |> Map.put(:header, rtn[:header] |> record_to_map())
      |> Map.put(
        :avps,
        rtn[:avps]
        |> Enum.map(fn x ->
          record_to_map(x)
        end)
      )
      |> Map.put(
        :errors,
        rtn[:errors]
        |> Enum.map(fn x ->
          case x do
            {e, avp} -> {e, record_to_map(avp)}
            v -> v
          end
        end)
      )
  end
end
