AccouontingRequest = {}
AccouontingRequest_mt = {__index = AccouontingRequest}

function AccouontingRequest:create(username, tags_list)
    local rtn = {
        tags_list = tags_list,
        username = "",
        properties = {}
    }
    setmetatable(rtn, AccouontingRequest_mt)
end
