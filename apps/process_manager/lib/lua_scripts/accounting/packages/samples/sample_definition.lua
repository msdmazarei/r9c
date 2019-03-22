require "accounting/packages/package_definition"
require "accounting/packages/package_instance"
require "utils"
require "utils/string"
require "accounting/ocs_account"
require "accounting/packages/user_packages"
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

function process_accounting_request(init_vars, acc_req)
    -- body
end

new_pkg_def = PackageDefinition:create(pkg_name, activation, deactive, process_accounting_request)

success, result = new_pkg_def:save()

if success == true then
    print("successfully package defined.")
else
    print("problem in saving package definition")
end
