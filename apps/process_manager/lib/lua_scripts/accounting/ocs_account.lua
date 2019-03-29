OCSAccount = {}
OCSAccount_mt = {__index = OCSAccount}
-- require("utils")
function OCSAccount:create(username, account_type, expiredate_epoch, amount)
    -- body
    -- print("create called.")
    local rtn = {
        username = username,
        account_type = account_type,
        additional_props = {},
        values = {
            [expiredate_epoch] = {amount = amount, data = {}}
        }, -- map of { expireepoch  = amount, expireepch = amount,....}
        _is_loaded_from_db = false
    }

    setmetatable(rtn, OCSAccount_mt)
    return rtn
end


function OCSAccount:create_with_data(username, account_type, expiredate_epoch, amount,data)
    -- body
    -- print("create called.")
    local rtn = {
        username = username,
        account_type = account_type,
        additional_props = {},
        values = {
            [expiredate_epoch] = {amount = amount, data = data}
        }, -- map of { expireepoch  = amount, expireepch = amount,....}
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
    -- print("account key:", key, "saved")
    local old_db_record = cel.ocs_account.get_for_update(key)
    -- print("after get for update")
    local new_db_record = self:get_db_value(old_db_record)
    -- print("after get_db_value")
    local n = cel.utils.unixepoch_now()
    new_db_record["last_update"] = n
    new_db_record:remove_before_when(n)
    local res = cel.ocs_account.set(key, new_db_record)
    if res == "ok" then
        return true, self
    else
        return false, res
    end
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
    -- print("OCSAccount:get_amount_till - self:",dump(self))
    for k, v in pairs(self.values) do
        -- print("get_amount_till loop on k:",k, "target epoch:",target_epoch)
       --sum over all which expire after target_epoch
        if k >= target_epoch then
            sum = sum + (v["amount"] or 0)
        end
    end
    -- print("OCSAccount:get_amount_till rtn:", sum)
    return sum
end

function OCSAccount:add(amount, when)
    local is_done = false
    local account_value = self.values[when] or {amount = 0, data = {}}
    account_value["amount"] = account_value["amount"] + amount
    self.values[when] = account_value
end

function OCSAccount:add_with_data(amount, when, data)
    self.values[when] = {
        amount = amount,
        data = data
    }
end

function OCSAccount:dec(amount)
    -- print("called")
    local remain = amount
    local updated_keys = {}
    for k, v in pairs(self.values) do
        if remain == 0 then
            break
        end

        local account_amount = v["amount"]

        if account_amount >= remain then
            updated_keys[k] = account_amount - remain
            print("k:",k,"updated to:",updated_keys[k])
            remain = 0
            break
        else
            updated_keys[k] = 0
            remain = remain - account_amount
        end
    end

    for k, v in pairs(updated_keys) do
        self.values[k]["amount"] = v
    end

    return remain 
end

function OCSAccount:remove_before_when(when)
    -- cel.utils.debug_print({before = self})
-- print("remove_before_when called.")
    local rtn = {}
    for k, v in pairs(self.values) do
        local account_amount = v["amount"] or 0
        -- print("remove_before_when checking k:",k,"and when:",when,", k>when",k>when,"amount:",account_amount)
    
        if k > when and account_amount > 0 then
            -- print("add it to rtn values k:v",k,dump(v))
            rtn[k] = v
        end
    end
    -- print("finally rtn:",dump(rtn))
    self.values = rtn
end

function OCSAccount.get_class_metatable()
    return OCSAccount_mt
end
