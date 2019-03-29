require "accounting/packages/package_definition"
require "accounting/packages/package_instance"
require "utils"
require "utils/string"
require "accounting/ocs_account"
require "accounting/packages/user_packages"
require "accounting/accounting_response"
require "accounting/accounting_request"

pkg_name = "regular_adsl_internet_ratio"

-- will return true, PackageInstance Object or false, error
function activation(username, identifier, priority, activation_props)
    -- we expect receive these fields in activation_props
    -- duration
    -- speed
    -- max traffic (send+recv)
    print("activation called.")
    print("username:", username)
    print("ident:", identifier)
    print("prio:", priority)
    print("act_pro", dump(activation_props))

    local props = activation_props or {}
    priority = priority or 1
    identifier = identifier or random_string(10)
    print("line26")
    if username == nil then
        return false, "no user name passed"
    end

    if props["duration"] == nil or props["speed"] == nil or props["traffic"] == nil then
        return false, "no proper activation properties passed"
    end
    print("line34")

    local expiredate = cel.utils.unixepoch_now() + props["duration"] * 3600 * 30
    local gateway_tags = {"fixed_internet"}
    print("line38")

    --get user packages
    local user_packages = UserPackages.get_for_update(username)
    if user_packages == nil then
        print("no user packages found, creating it")
        user_packages = UserPackages:create(username, {})
    end
    --check that already the package is enabled for user
    local defined_package = user_packages:get_package_by_name_and_identifier(pkg_name, identifier)
    if defined_package ~= nil then
        return false, "already assigned"
    end
    --package instance
    local pkg_instance =
        PackageInstance:create(
        pkg_name,
        username,
        identifier,
        priority,
        expiredate,
        gateway_tags,
        {
            speed = props["speed"],
            multi_session = false
        }
    )
    print("line56")
    user_packages:add_package(pkg_instance)
    success, result = user_packages:save()
    if success == false then
        return success, result
    end
    print("user packages are added.")

    --charge user accounts
    local account_type = "fixed_internet_traffic"
    print("getting Account")
    local user_account = OCSAccount.get_for_update(username, account_type)
    if user_account == nil then
        print("creating Account")

        user_account =
            OCSAccount:create_with_data(
            username,
            account_type,
            expiredate,
            props["traffic"],
            {
                identifier = identifier,
                pkg_name = pkg_name
            }
        )
    else
        --check
        user_account:add_with_data(
            props["traffic"],
            expiredate,
            {
                identifier = identifier,
                pkg_name = pkg_name
            }
        )
    end

    print("line 44")

    success, res = user_account:save()

    if success == false then
        return success, res
    end

    print("successfully defined.")
    return true, pkg_instance
end

function deactive(username, identifier)
end

function reject_cause_of_not_account(acc_req)
    print("sample definition - user has no account to connect")
    local acc_res = AccountingResponse:create(400, acc_req.username, {}, {})

    if acc_req.gateway_model == "mikrotik" then
        acc_res.result_props["code"] = "ok"
        acc_res.result_props["desc"] = "no account"
    end

    print("returns acc_res rejected")
    return acc_res
end

function accept_connection(pkg_instance, acc_req)
    print("accept_connection called.")
    local acc_res = AccountingResponse:create(200, acc_req.username, {}, {})
    if acc_req.gateway_model == "mikrotik" then
        acc_res.result_props["speed"] = pkg_instance["accounting_request_processing_data"]["speed"] or 1024
    end
    return acc_res
end

function accept_acct_req(pkg_instance, acc_req)
    print("accept_acct_req called.")
    local acc_res = AccountingResponse:create(200, acc_req.username, {}, {})
    if acc_req.gateway_model == "mikrotik" then
        acc_res.result_props["acct_accept"] = "ok"
    end
    return acc_res
end

function process_accounting_request(pkg_instance, acc_req)
    local SECOND = 1000

    print("process_accounting_request called.")
    print("acc_req:", dump(acc_req))
    print("pkg_instance:", dump(pkg_instance))
    local account_type = "fixed_internet_traffic"

    if acc_req.request_type == "start" then
        print("request type is start")
        --check user has account to connect
        local user_acc = OCSAccount.get(acc_req.username, account_type)
        if user_acc == nil then
            acc_res = reject_cause_of_not_account(acc_req)
            return false, acc_res
        end

        if user_acc:get_amount_till(cel.utils.unixepoch_now() + 10 * SECOND) <= 0 then
            acc_res = reject_cause_of_not_account(acc_req)
            return false, acc_res
        end
        return true, accept_connection(pkg_instance, acc_req)
    end

    if acc_req.request_type == "intrim" then
        print("request type is intrim")
        local user_acc = OCSAccount.get(acc_req.username, account_type)
        if user_acc == nil then
            acc_res = reject_cause_of_not_account(acc_req)
            return false, acc_res
        end

        if user_acc:get_amount_till(cel.utils.unixepoch_now() + 10 * SECOND) <= 0 then
            acc_res = reject_cause_of_not_account(acc_req)
            return false, acc_res
        end

        local uploaded_bytes = 0
        local downloaded_bytes = 0

        -- gateway specific
        if acc_req.gateway_model == "mikrotik" then
            uploaded_bytes = acc_req.gateway_accounting_props["UPLOADED"]
            downloaded_bytes = acc_req.gateway_accounting_props["DOWNLOADED"]
        end

        local total_bytes = uploaded_bytes + downloaded_bytes
        print("intrim - total bytes:", total_bytes)
        local remains = user_acc:dec(total_bytes)
        print("intrim - remains:", remains)
        if remains == 0 then
            user_acc:save()
            return true, accept_acct_req(pkg_instance, acc_req)
        else
            -- pass remains to next package
            -- to do
            print("generating new accounting request")
            local new_acc_req =
                AccountingRequest:create(
                acc_req.username,
                acc_req.request_type,
                acc_req.gateway_model,
                acc_req.gateway_tags_list,
                acc_req.gateway_accounting_props,
                acc_req.aditional_props
            )
            new_acc_req.gateway_accounting_props["UPLOADED"] = remains / 2
            new_acc_req.gateway_accounting_props["DOWNLOADED"] = remains / 2
            new_acc_req.aditional_props = new_acc_req.aditional_props or {}
            new_acc_req.aditional_props["orig_acc_req"] = acc_req

            local acc_res = reject_cause_of_not_account(acc_req)
            print("return false and table (req,res)")
            return false, {
                request = new_acc_req,
                response = acc_res
            }
        end
    end

    if acc_req.request_type == "stop" then
    end

    print("process_accounting_request - unknown accounting type")
    return false, "accounting request type is unknown"
end
function is_expired(username, pkg_instance)
end
new_pkg_def = PackageDefinition:create(pkg_name, activation, deactive, process_accounting_request)

success, result = new_pkg_def:save()

if success == true then
    print("successfully package defined.")
else
    print("problem in saving package definition")
end
