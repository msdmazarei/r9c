AccountingResponse = {__msd_class_name = "AccountingResponse"}
AccountingResponse_mt = {__index = AccountingResponse, __msd_class_name = "AccountingResponse"}

function AccountingResponse:create(result_code, username, result_props, additional_props)
    local rtn = {
        result_code = result_code,
        username = username,
        additional_props = additional_props,
        result_props = result_props,
        __msd_class_name = "AccountingResponse"
    }
    setmetatable(rtn, AccountingResponse_mt)
    return rtn
end

function AccountingResponse.get_class_metatable()
    return AccountingResponse_mt
end
