require "accounting/packages/package_definition"
require "accounting/packages/package_instance"

require "utils/string"
pkg_name = "regular_adsl_internet_ratio"
function activation(username, identifier, priority, activation_props)
    -- we expect receive these fields in activation_props
    -- duration
    -- speed
    -- max traffic (send+recv)
    print("activation called.")

    local props = activation_props or {}
    priority = priority or 1
    identifier = identifier or random_string(10)

    if username == nil then
        return false, "no user name passed"
    end

    if props["duration"] == nil or props["speed"] == nil or props["traffic"] == nil then
        return false, "no proper activation properties passed"
    end

    local expiredate = cel.utils.unixepoch_now() + props["duration"] * 3600 * 30
    local gateway_tags = {"fixed_internet"}

    --charge user accounts
    local account_type = "fixed_internet_traffic"
    local user_account = OCSAccount:create(user_account, account_type, expiredate, props["traffic"])

    --package instance
    local pkg_instance =
        PackageInstance:create(pkg_name, username, identifier, priority, expiredate, gateway_tags, {speed = props["speed"]})

    return cel.kvdb.transaction(
        function()
            user_account:save()
            pkg_instance:save()
        end,
        {},
        "infinity"
    )
end

function deactive(username, identifier)
end

function process_accounting_request(init_vars, acc_req)
    -- body
end

new_pkg_def = PackageDefinition:create(pkg_name, activation, deactive, process_accounting_request)

new_pkg_def:save()
