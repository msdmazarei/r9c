require("utils/reflection")
require("diameter/diameter_avp")
require("diameter/diameter_avp_commands")

-- DiameterModelFromRC9 = {
--     id = nil,
--     client_address = nil,
--     client_port = nil,
--     capture_timestamp = nil,
--     packet_bin = "",
--     parsed_packet = nil, -- DiameterPacket
--     options = {}
-- }

-- DiameterPacket = {
--     version = 1,
--     message_length = 0,
--     request_bit = 0,
--     proxiable_bit = 0,
--     error_bit = 0,
--     potentially_retransmit = 0,
--     command_code = 0,
--     application_id = 0,
--     hop_by_hop_id = 0,
--     end_to_end_id = 0,
--     avps = {} -- list of diameter AVP
-- }

DiameterPacket = {}
DiameterPacket_mt = {__index = DiameterPacket}

function DiameterPacket.response_from_resquest(request)
    return DiameterPacket:create {
        version = request.version,
        request_bit = 0,
        proxiable_bit = request.proxiable_bit,
        error_bit = 0,
        potentially_retransmit = 0,
        command_code = DIAMETER_COMMANDS.get_response_for_request(request.command_code),
        application_id = request.application_id,
        hop_by_hop_id = request.hop_by_hop_id,
        end_to_end_id = request.end_to_end_id
    }
end

function DiameterPacket:create(t)
    local new_inst = {}
    setmetatable(new_inst, DiameterPacket_mt)

    if type(t) ~= "table" then
        t = {}
    end

    new_inst.version = t.version or 1
    new_inst.message_length = t.message_length or 0
    new_inst.request_bit = t.request_bit or 0
    new_inst.proxiable_bit = t.proxiable_bit or 0
    new_inst.error_bit = t.error_bit or 0
    new_inst.potentially_retransmit = t.potentially_retransmit or 0
    new_inst.command_code = t.command_code or 0
    new_inst.application_id = t.application_id or 0
    new_inst.hop_by_hop_id = t.hop_by_hop_id or 0
    new_inst.end_to_end_id = t.end_to_end_id or 0
    new_inst.avps = t.avps or {} -- list of diameter AVP
    
    --useful when elixir process tries to deserialze maps
    new_inst.__struct__ = "Elixir.Utilities.Parsers.Diameter.DiameterPacket" 

    return new_inst
end

function DiameterPacket:add_avp(avp)
    if is_instanceof(avp, DiameterAVP) == false then
        error("passed argument is not avp")
    end
    self:del_avp(avp.avp_code)
    table.insert(self.avps, avp)
end

--returns a pair as index, avp value
function DiameterPacket:get_avp_index(avp_code)
    for i, avp in ipairs(self.avps) do
        if avp.avp_code == avp_code then
            return i
        end
    end
    return nil
end

function DiameterPacket:get_avp(avp_code)
    local avp_index = self:get_avp_index(avp_code)
    if avp_index == nil then
        return nil
    end
    return self.avps[avp_index]
end

function DiameterPacket:del_avp(avp_code)
    local avp_index = self:get_avp_index(avp_code)
    if avp_index == nil then
        return nil
    end
    table.remove(self.avps, avp_index)
    return avp_index
end
