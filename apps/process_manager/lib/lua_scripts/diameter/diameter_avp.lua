require "diameter/diameter_avp_type"

DiameterAVP = {}
DiameterAVP_mt = {__index = DiameterAVP}

--- to create an avp its possible to pass a table with
--- avp_code
--- vendor_specific
--- mandatory
--- protected
--- avp_type
--- avp_value
--- bin_value
--- avp_type is enforced

function DiameterAVP:create(t)
    local new_inst = {}
    setmetatable(new_inst, DiameterAVP_mt)

    if type(t) ~= "table" then
        t = {}
    end

    new_inst.avp_type = t.avp_type or nil

    if new_inst.avp_type == nil then
        error("avp_type is forced to specify. you must pass it")
    end

    if DIAMETER_AVP_TYPE[new_inst.avp_type] == nil then
        error("your spceified AVP Type is not supported")
    end

    new_inst.avp_code = t.avp_code or 0
    new_inst.vendor_specific = t.vendor_specific or 0
    new_inst.mandatory = t.mandatory or 0
    new_inst.protected = t.protected or 0
    new_inst.avp_value = t.avp_value or nil


    --useful when elixir process tries to deserialze maps
    new_inst.__struct__ = "Elixir.Utilities.Parsers.Diameter.AVP" 

    return new_inst
end

function DiameterAVP:set_value(value)

    if DIAMETER_AVP_TYPE.check_type(self.avp_type, value) == false then
        error("value is invalid to store in this avp. cause of its value is incompatible with avp type")
    end
    
    self.avp_value = value
end

function DiameterAVP.get_class_metatable()
    return DiameterAVP_mt
end

function DiameterAVP:get_uint32_value()
    local val = self.bin_value
    local rtn = string.byte(val,1)* 0x1000000 + string.byte(val,2)*0x10000+ string.byte(val,3) * 0x100 + string.byte(val,4)
    return  rtn
end