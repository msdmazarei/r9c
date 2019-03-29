require "utils/list"
require "utils/reflection"
require "accounting/packages/package_instance"
require "utils"

UserPackages = {}
UserPackages_mt = {__index = UserPackages, __msd_class_name = "UserPackages"}

function UserPackages.get_class_metatable()
    return UserPackages_mt
end

function UserPackages:create(username, active_packages)
    local rtn = {
        username = username,
        active_packages = active_packages,
        __msd_class_name = "UserPackages"
    }
    setmetatable(rtn, UserPackages_mt)
    return rtn
end

function UserPackages.get_db_key(username)
    return "user_packages_" .. username
end

function UserPackages:remove_expired_ones()
    self.active_packages =
        list_filter(
        function(item)
            if is_instanceof(item, PackageInstance) then
                return item:is_expired() == false
            else
                print(
                    [[
                        wierd item found in active_packages
                        it is no PackageIstance Type!!!
                        WTF!
                    ]]
                )
                return false
            end
        end,
        self.active_packages
    )
end
function UserPackages.get_for_update(username)
    local key = UserPackages.get_db_key(username)
    local rtn = cel.kvdb.get_for_update(key)
    if rtn == nil then
        return rtn
    else
        setmetatable(rtn, UserPackages_mt)
        return rtn
    end
end

function UserPackages.get_from_db(username)
    local key = UserPackages.get_db_key(username)
    local rtn = cel.kvdb.get(key)
    if rtn == nil then
        return rtn
    else
        setmetatable(rtn, UserPackages_mt)
        return rtn
    end
end

function UserPackages:save()
    print("UserPackages:save called.")
    print(dump(self.username))
    local key = UserPackages.get_db_key(self.username)
    return cel.kvdb.set(key, self)
end
function UserPackages:remove_package(pkg_identifier)
    print("UserPackages:remove_package called.")
    self.active_packages =
        list_filter(
        function(item)
            return item.identifier ~= pkg_identifier.identifier
        end,
        self.active_packages
    )
end

function UserPackages:add_package(package_instance)
    print("UserPackages:add_package called.")
    self:remove_package(package_instance.identifier)
    print("package removed")

    self.active_packages = list_append(self.active_packages, package_instance)
end
function UserPackages:get_package_by_name_and_identifier(packagedef_name, identifier)
    local rtn =
        list_filter(
        function(item)
            if item.packagedef_name == packagedef_name and item.identifier == identifier then
                return true
            else
                return false
            end
        end,
        self.active_packages
    )
    if list_length(rtn) > 0 then
        return rtn[1]
    else
        return nil
    end
end
