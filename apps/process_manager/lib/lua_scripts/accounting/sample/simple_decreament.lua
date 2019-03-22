require "accounting/ocs_account"
return cel.ocs_account.transaction(
    function() 
        local username ="msd1"
        local account_type = "internet_bytes"
        local msd_internet_bytes_account = OCSAccount.get_for_update(username,account_type)

        if msd_internet_bytes_account == nil then 
           return false
        end

        local rtn = msd_internet_bytes_account:dec(353) 

        msd_internet_bytes_account:save()
        return rtn

	end, {}, "infinity")
