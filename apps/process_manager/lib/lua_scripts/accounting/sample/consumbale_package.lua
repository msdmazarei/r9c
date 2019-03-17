require "accounting/cosumable_package"
require "accounting/accounting_request"
require "accounting/accounting_response"
require "utils/list"
require "utils"
function process_request(ar)
    print("process_request Called.")
    print("input:",dump(ar))
    local rtn = AccountingResponse:create("ok", ar.username, {})
    if list_contains(ar.tags_list, "test") then
        rtn = AccountingResponse:create("ok", ar.username, {test = "done"})
    end
    print("process_request returned.")

    return rtn
end
print("before create")
local cp = ConsumablePackage:create("msd_test", {"test"}, process_request, nil)
print(cp)
return cp:save()
