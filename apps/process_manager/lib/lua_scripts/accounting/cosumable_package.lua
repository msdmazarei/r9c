require("accounting/accounting_request")
require("accounting/accounting_response")
require("utils/reflection")

ConsumablePackage = {}
ConsumablePackage_mt = {__index = ConsumablePackage}

function ConsumablePackage:create(name, tags, script, expiration_epoch_delta)
    print("ConsumablePackage:create Called.")
    local rtn = {
        name = name,
        tags = tags,
        -- should contains a function whose name is process_request that accepts AccountRequest
        -- and returns AccountingResponse
        -- to check true compilation if its input was an accounting request that contains "test_compile"
        -- tag it returns accountingresponse with additional_property of "test" => "done"
        script = script,
        expiration_epoch_delta = expiration_epoch_delta,
        account_increases = {}, -- table { account_name = {exp_delta, amount} }
        compiled_state = nil
    }
    setmetatable(rtn, ConsumablePackage_mt)
    return rtn
end

function ConsumablePackage:compile_script()
    local compiled_code = cel.utils.compiled_code(self.script)
    if compiled_code == false then
        return false
    else
        self.compiled_state = compiled_code
        return true
    end
end

function ConsumablePackage:run_script(accouting_request)
    print("ConsumablePackage:run_script Called.")
    if type(self.script) == "function" then
        return true, self.script(accouting_request)
    end
    if self.compiled_state == nil then
        if self:compile_script() == false then
            return false, nil
        end
    end
    local status, result = cel.utils.call_function_in_state("process_request", {accouting_request}, self.compiled_state)
    return status, result
end

function ConsumablePackage.get_db_key(name)
    return "consumable_package_" .. name
end
function ConsumablePackage:save()
    print("ConsumablePackage:save() Called.")
    local test_request = AccountingRequest:create("test", {"test"})
    local stat, res = self:run_script(test_request)
    print("run_script returned stat:",stat)
    print("res:",dump(res))
    if stat == false then
        return false, "compile error"
    end
    --checking response of object
    print("is_instanceof function")
    if is_instanceof(res, AccountingResponse) == false then
        print("64")
        return false, "code return value type error"
    end
    print("68")
    if res.additional_props["test"] ~= "done" then
        print("68")
        return false, "code return value not proper we expected"
    end
    print("71")

    local key = ConsumablePackage.get_db_key(self.name)
    cel.kvdb.set(key, self)
    return true, nil
end


function ConsumablePackage.get_class_metatable()
    return ConsumablePackage_mt
end