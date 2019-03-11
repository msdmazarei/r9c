require "diameter/diameter_avp"
require "diameter/diameter_avp_code"
require "diameter/diameter_avp_code_type"


function diameter_result_code(value)
    local rtn_avp_code = DIAMETER_AVP_CODE.Result_Code
    local rtn = DiameterAVP:create{
        avp_code = rtn_avp_code ,
        avp_type = DIAMETER_AVP_CODE_TYPE[ rtn_avp_code ]
    }
    rtn:set_value(value)
    return rtn
end

function general_uint32_avp(code,value) 
    local rtn = DiameterAVP:create{
        avp_code = code ,
        avp_type = DIAMETER_AVP_TYPE.Unsigned32
    }
    rtn:set_value(value)
    return rtn
end