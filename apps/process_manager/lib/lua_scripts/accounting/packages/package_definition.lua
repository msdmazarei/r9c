PackageDefinition = {}
PackageDefinition_mt = {__index = PackageDefinition, __msd_class_name = "PackageDefinition"}

function PackageDefinition.get_class_metatable()
    return PackageDefinition_mt
end

-- activation_func : a function which has form: function( username: string, identifier: string,
--                                                        priority: number, activation_props: table )
--                                                 : PackageInstance
--                  it will be called when we want to assign a package to specific username
--                  its duty is charge user accounts and create PackageInstance for user
--                  activation props are some properties which will pass to accounting_process_func
--                  like internet_speed or internet traffic or duration(expire date) and  so on ..
--                  and will returns PackageInstance
-- deactivation_func: a function has form: function(username: string, identifier: string). it will be called
--                      when we want to expire a package instance
-- accouting_request_process_func: a function has form:: function ( init: PackageInstance , accounting_request: AccountingRequest) : AccountingResponse
function PackageDefinition:create(name, activation_func, deactivation_func, accounting_process_func)
    local rtn = {
        name = name,
        activation_func = activation_func,
        deactivation_func = deactivation_func,
        accounting_process_func = accounting_process_func
    }
    setmetatable(rtn, PackageDefinition_mt)
    return rtn
end
function PackageDefinition.get_db_key(name)
    return "package_definition_" .. name
end
function PackageDefinition:save()
    local key = PackageDefinition.get_db_key(self.name)
    result = cel.kvdb.set(key, self)
    if result=="ok" then
        return true, self
    else
        return false, result
    end
end
function PackageDefinition.get_from_db(name)

    local key = PackageDefinition.get_db_key(name)
    print("calling kvdb.get")
    local rtn =  cel.kvdb.get(key)
    print("retuen value")

    if rtn ~= nil then
        setmetatable(rtn, PackageDefinition_mt)
    end
    return rtn
end

function PackageDefinition:assign_to_user( username, identifier, priority, activation_props )
    print("assign to user called. for user:",username)
    print("type of activation_func:",type(self.activation_func))
    return self.activation_func(username, identifier, priority, activation_props)
end
