AccountingRequest = {}
AccountingRequest_mt = {__index = AccountingRequest}

function AccountingRequest:create(username, tags_list,props)
    local rtn = {
        tags_list = tags_list,
        username = "",
        additional_props = props
    }
    setmetatable(rtn, AccountingRequest_mt)
    return rtn
end
function AccountingRequest.get_class_metatable()
    return AccountingRequest_mt
end