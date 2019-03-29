require "diameter/diameter_packet"
require "diameter/diameter_general_avp"
require "diameter/diameter_avp"
require "utils"
require "accounting/accounting_request"
local response = DiameterPacket.response_from_resquest(cel.incoming_message.parsed_packet)

local dia_in = cel.incoming_message.parsed_packet
setmetatable(dia_in, DiameterPacket_mt)
local username = dia_in:get_avp(1)
if username ~= nil then
    print("username:", username:get_string_value())
    for _, avp_code in pairs({263, 260, 264, 296, 277, 3}) do
        local req_avp = dia_in:get_avp(avp_code)
        if req_avp ~= nil then
            response:add_avp(req_avp)
        end
    end
    response:add_avp(diameter_result_code(2001))
else
    print("no username passed")
    response:add_avp(diameter_result_code(4001))
end

return response
