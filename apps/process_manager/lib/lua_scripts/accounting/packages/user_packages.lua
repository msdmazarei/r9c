require "utils/list"
require "utils/reflection"
require "accounting/packages/package_instance"

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
function UserPackages:get_for_update()
    local key = UserPackages.get_db_key(self.username)
    local rtn = cel.kvdb.get_for_update(key)
    if rtn == nil then
        return rtn
    else
        setmetatable(rtn, UserPackages_mt)
        return rtn
    end
end
function UserPackages:save()
 
            local key = UserPackages.get_db_key(self.username)
            local old_db_record = self:get_for_update()
            return cel.kvdb.set(key, self)
      
end
function UserPackages:remove_package(pkg_identifier)
    self.active_packages =
        list_filter(
        function(item)
            return item.identifier ~= package_instance.identifier
        end,
        self.active_packages
    )
end

function UserPackages:add_package(package_instance)
    self:remove_package(package_instance.identifier)
    self.active_packages = list_append(self.active_packages, package_instance)
end

function UserPackages:activate_package( package_def,identifier, priority, activation_data )

end
    -- body
end
