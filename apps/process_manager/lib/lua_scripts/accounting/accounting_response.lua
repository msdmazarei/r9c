AccountingResponse = {}
AccountingResponse_mt = {__index = AccountingResponse, __msd_class_name="AccountingResponse"}

function AccountingResponse:create(result_code, username, prop_list)
    local rtn = {
        result_code = result_code,
        username = username,
        additional_props = prop_list
    }
    setmetatable(rtn, AccountingResponse_mt)
    return rtn
end

function AccountingResponse.get_class_metatable()
    return AccountingResponse_mt
end

