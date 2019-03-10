require "diameter/diameter_packet"
require "diameter/diameter_general_avp"

cel = {
    incoming_message = {
        parsed_packet = {}
    }
}

local response = DiameterPacket.response_from_resquest(cel.incoming_message.parsed_packet)
response:add_avp(diameter_result_code(2000))

