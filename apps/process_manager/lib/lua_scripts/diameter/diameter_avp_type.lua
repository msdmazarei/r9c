require "utils/math"

DIAMETER_AVP_TYPE = {
    -- 4.2 RFC 3588 Basic AVP Data Formats
    OctetString = "OctetString",
    Integer32 = "Integer32",
    Integer64 = "Integer64",
    Unsigned32 = "Unsigned32",
    Unsigned64 = "Unsigned64",
    Float32 = "Float32",
    Float64 = "Float64",
    -- 4.3 RFC 3588 Derived AVP Data Formats
    Address = "Address",
    Time = "Time",
    UTF8String = "UTF8String",
    DiameterIdentity = "DiameterIdentity",
    DiameterURI = "DiameterURI",
    Enumerated = "Enumerated",
    IPFilterRule = "IPFilterRule",
    QoSFilterRule = "QoSFilterRule"
}

function DIAMETER_AVP_TYPE.check_type(type_to_check, value)
    if (type_to_check == DIAMETER_AVP_TYPE.Unsigned32) or (type_to_check == DIAMETER_AVP_TYPE.Unsigned64) then
        return is_number(value) and (is_positive(value) or value == 0) and is_integer(value)
    end
    if(type_to_check==DIAMETER_AVP_TYPE.OctetString) then
        return type(value) == "string"
    end
    return false
end
