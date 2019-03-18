PackageInstance = {}
PackageInstance_mt = {__index = PackageInstance, __msd_class_name = "PackageInstance"}

function PackageInstance.get_class_metatable()
    return PackageInstance_mt
end

function PackageInstance:create(
    packagedef_name,
    username,
    identifier,
    priority,
    expire_epoch,
    gateway_tags,
    accounting_request_processing_data)
    local rtn = {
        packagedef_name = packagedef_name,
        username = username,
        identifier = identifier,
        priority = priority,
        expire_epoch = expire_epoch,
        gateway_tags = gateway_tags,
        accounting_request_processing_data = accounting_request_processing_data
    }
    setmetatable(rtn, PackageInstance)
    return rtn
end

function PackageInstance.get_db_key(username)
    return "userpackages_" .. username
end

function PackageInstance:is_expired()
    -- TODO: better check will contains user account remains
    return cel.utils.unixepoch_now() > self.expire_epoch 
    
end