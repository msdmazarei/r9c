require "accounting/packages/user_packages"
require "accounting/packages/package_definition"
require "utils/list"
require "utils/reflection"
require "accounting/accounting_response"
require "utils"

AccountingRequest = {}
AccountingRequest_mt = {__index = AccountingRequest}

function AccountingRequest:create(
    username,
    request_type,
    gateway_model,
    gateway_tags_list,
    gateway_accounting_props,
    aaditional_props)
    local rtn = {
        gateway_tags_list = gateway_tags_list or {},
        username = username,
        request_type = request_type,
        gateway_accounting_props = gateway_accounting_props or {},
        additional_props = aaditional_props or {},
        gateway_model = gateway_model
    }
    setmetatable(rtn, AccountingRequest_mt)
    return rtn
end
function AccountingRequest.get_class_metatable()
    return AccountingRequest_mt
end


function AccountingRequest:process()
    --get user packages
    --filter packages by gatwaytag
    --sort by priotity
    --extract first
    --excute it processing
    --if it fully covered return true
    --else it couldnt pass remain to next one
    --till all tg

    local user_packages = UserPackages.get_from_db(self.username)
    if user_packages == nil then
        print("no package found for user")
        return false, "no package found for user"
    end
    print("filtering valid packages")
    local valid_packages =
        list_filter(
        function(item)
            return list_has_shared_item(item.gateway_tags or {}, self.gateway_tags_list or {})
        end,
        user_packages.active_packages
    )
    if list_length(valid_packages) == 0 then
        print("no valid packages found for request")
        return false, "no valid packages found for request"
    end

    print("sorting")
    table.sort(
        valid_packages,
        function(a, b)
            print("sort function called")
            print("a:", dump(a))
            print("b:", dump(b))
            print("a[priority]", a["priority"])
            print("b[priority]", b["priority"])
            print("a[expire_epoch]", a["expire_epoch"])
            print("b[expire_epoch]", a["expire_epoch"])
            if a["priority"] == b["priority"] then
                return a["expire_epoch"] < b["expire_epoch"]
            end
            return a["priority"] < b["priority"]
        end
    )
    print("sorted")
    local acct_req = self
    local acct_res = nil
    for _, pkg_instance in pairs(valid_packages) do
        local pkg_def = PackageDefinition.get_from_db(pkg_instance.packagedef_name)
        local success, result = 
            pkg_def:process_acct(pkg_instance, acct_req)
        
        print("AccountingRequest:process - after processing acct , success, result:",success,result)
        print(dump(result))
        if success == false and type(result) == "string" then
            print("error happend, error:", result)
            return success, result
        end
        if success == true then
            print("successfully accounted")
            return success, result
        end
        if success == false and is_instanceof(result, AccountingResponse) then
            print("false and error accounting response")
            return success, result
        end
        if success == false and type(result) == "table" then
            print("partially succeed and pass to next package")
            acct_req = result["request"]
            acct_res = result["response"]
        end
    end
                -- should expire current package because has no account right now


    return true, acct_res
end
