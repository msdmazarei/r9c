ConsumablePackage = {}
ConsumablePackage_mt = {__index=  ConsumablePackage}

function  ConsumablePackage:create()
    local rtn = {

    }
    setmetatable(rtn, ConsumablePackage_mt)
    return rtn
end