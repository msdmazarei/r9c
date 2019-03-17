OCSAccount = {}
OCSAccount_mt = {__index = OCSAccount}

function OCSAccount:create(username, account_type, expiredate_epoch, amount)
    -- body
    -- print("create called.")
    local rtn = {
        username = username,
        account_type = account_type,
        additional_props = {},
        values = {[expiredate_epoch] = amount}, -- map of { expireepoch  = amount, expireepch = amount,....}
        _is_loaded_from_db = false
    }

    setmetatable(rtn, OCSAccount_mt)
    return rtn
end

function OCSAccount:get_db_key()
    return self.username .. "_" .. self.account_type
end

function OCSAccount:get_db_value(old_db_record)
    -- print("get_db_value called")
    if old_db_record ~= nil then
        -- prevent store whole history of record in table
        -- only last one
        old_db_record["old_record"] = nil
    end

    local rtn = {
        username = self.username,
        account_type = self.account_type,
        additional_props = self.additional_props,
        old_record = old_db_record,
        values = self.values or old_db_record or {}
    }
    setmetatable(rtn, OCSAccount_mt)
    -- print("line 37")
    return rtn
end

function OCSAccount:save()
    local key = self:get_db_key()
    local old_db_record = cel.ocs_account.get_for_update(key)
    local new_db_record = self:get_db_value(old_db_record)
    local n = cel.utils.unixepoch_now()
    new_db_record["last_update"] = n
    new_db_record:remove_before_when(n)
    cel.ocs_account.set(key, new_db_record)
    return true
end

function OCSAccount.get(username, account_type)
    local key = username .. "_" .. account_type
    local db_record = cel.ocs_account.get(key)
    if db_record ~= nil then
        setmetatable(db_record, OCSAccount_mt)
        db_record._is_loaded_from_db = true
        db_record:remove_before_when(cel.utils.unixepoch_now())
    end
    return db_record
end

function OCSAccount.get_for_update(username, account_type)
    local key = username .. "_" .. account_type
    local db_record = cel.ocs_account.get_for_update(key)
    if db_record ~= nil then
        setmetatable(db_record, OCSAccount_mt)
        db_record._is_loaded_from_db = true
        db_record:remove_before_when(cel.utils.unixepoch_now())
    end
    return db_record
end

function OCSAccount:get_amount_till(target_epoch)
    local sum = 0
    for k, v in ipairs(self.values) do
        if k <= target_epoch then
            sum = sum + v
        end
    end
    return sum
end

function OCSAccount:add(amount, when)
    local is_done = false
    self.values[when] = (self.values[when] or 0) + amount
end
function OCSAccount:dec(amount)
    print("called")
    local remain = amount
    local updated_keys = {}
    for k, v in pairs(self.values) do
        if remain == 0 then
            break
        end

        if v >= remain then
            updated_keys[k] = v - remain
            remain = 0
            break
        else
            updated_keys[k] = 0
            remain = remain - v
        end
    end

    for k, v in pairs(updated_keys) do
        self.values[k] = v
    end

    return remain == 0
end

function OCSAccount:remove_before_when(when)
    -- cel.utils.debug_print({before = self})
    local rtn = {}
    for k, v in pairs(self.values) do
        if k > when and v > 0 then
            rtn[k] = v
        end
    end
    self.values = rtn
end


function OCSAccount.get_class_metatable( )
    return OCSAccount_mt
end