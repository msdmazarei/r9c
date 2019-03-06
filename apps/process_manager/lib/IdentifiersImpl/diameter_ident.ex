defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.DiameterPacket
  ] do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def get_config_for_client(client_address) do
    main_config = %{
      "127.0.0.1" => %{
        1 => %{
          "process_name" => %{
            "type" => "avp",
            "avp_code" => 1
          }
        }
      },
      "default" => %{
        1 => %{
          "process_name" => %{
            "type" => "avp",
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
            Logging.debug("no target avp (~p) found in packetd avps. return nil ", [avp_to_find])
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
    ""
  end
end
