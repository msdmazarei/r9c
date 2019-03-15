defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.DiameterPacket
  ] do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def get_process_module(%DatabaseEngine.Models.DiameterPacket{}) do
    ProcessManager.Process.DiameterProcess
  end

  def get_config_for_client(client_address) do
    main_config = %{
      "117.0.0.2" => %{
        1 => %{
          "process_name" => %{
            "type" => "avp",
            "avp_code" => 1
          }
        }
      },
      "default" => %{
        # for NASREQ application
        1 => %{
          "process_name" => %{
            "type" => "avp",
            # specifies UserName AVP
            "avp_code" => 1
          }
        },
        # for any app which not specified here
        "default" => %{
          # process name calculation
          "process_name" => %{
            # says calculate process name by avp is contaid in packet
            "type" => "avp",
            # avp code which specifies process name here for NASREQ packet means USERNAME
            "avp_code" => 1
          }
        }
      }
    }

    client_config = main_config[client_address] || main_config["default"] || %{}
    client_config
  end

  def get_config_for_app(app, client_conf) do
    client_conf[app] || client_conf["default"] || %{}
  end

  def get_process_name(
        _data = %DatabaseEngine.Models.DiameterPacket{
          client_address: client_address,
          parsed_packet: parsed_packet
        }
      ) do
    if parsed_packet == nil do
      Logging.debug("parsed_packet is null!!!")
      nil
    else
      clinet_conf = get_config_for_client(client_address)
      app_config = get_config_for_app(parsed_packet.application_id, clinet_conf)

      if app_config["process_name"] == nil do
        Logging.debug("no process_name defined in app_config. no idea how to calc it")
        nil
      else
        if app_config["process_name"]["type"] == "avp" do
          # filter avps of packet to find avp

          avp_to_find = app_config["process_name"]["avp_code"]

          target_avp =
            parsed_packet.avps
            |> Enum.filter(fn x -> x.avp_code == avp_to_find end)

          if length(target_avp) > 0 do
            t1 = target_avp |> hd()
            pname = t1.bin_value
            Logging.debug("process name calculated successfully. it is: ~p", [pname])
            pname
          else
            Logging.debug("no target avp (~p) found in packetd avps . return nil ", [avp_to_find])
            nil
          end
        else
          Logging.debug(
            "there is no known method to calculate process_name for diameter packet. process_name type in app_config is: ~p",
            [
              app_config["process_name"]["type"]
            ]
          )

          nil
        end
      end
    end
  end

  def get_identifier(
        _data = %DatabaseEngine.Models.DiameterPacket{
          id: id
        }
      ) do
    id
  end

  def get_script(_data, _state) do
    """

    require "diameter/diameter_packet"
    require "diameter/diameter_general_avp"
    require "diameter/diameter_avp"
    require "utils"

    local response = DiameterPacket.response_from_resquest(cel.incoming_message.parsed_packet)

    local dia_in = cel.incoming_message.parsed_packet

    setmetatable(dia_in, DiameterPacket_mt)

    print("called..........")
    local sip_msg_avp  = dia_in:get_avp(406)
    local sip_service_type = dia_in:get_avp(404)
    local avp_realm = dia_in:get_avp(283)
    local avp_nas_filter = dia_in:get_avp(400)
    local avp_user_name = dia_in:get_avp(1)



    print("==========================")
    print(avp_realm:get_string_value())


    local avp_value = sip_msg_avp:get_uint32_value()
    local sip_service_type_value = sip_service_type:get_uint32_value()

    print("sip msgid value:",avp_value)
    print("service type value: ", sip_service_type_value)


    local AVP_Service_Type = 404

    response:add_avp(diameter_result_code(2001))
    response:add_avp(general_uint32_avp(406,avp_value))
    response:add_avp(general_uint32_avp(AVP_Service_Type,sip_service_type_value))
    response:add_avp(general_octet_string_avp(283,"127.0.0.1"))
    response:add_avp(avp_nas_filter)

    if avp_user_name ~= nil then
      response:add_avp(avp_user_name)
    end
    return response


    """
  end
end
