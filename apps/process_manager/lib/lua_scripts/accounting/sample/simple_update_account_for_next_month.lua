require "accounting/ocs_account"
return cel.ocs_account.transaction(
    function() 
        local username ="msd"
        local account_type = "internet_bytes"
        local msd_internet_bytes_account = OCSAccount.get_for_update(username,account_type)
        next_30days_epoch = cel.utils.unixepoch_now() + 30*3600*1000

        if msd_internet_bytes_account == nil then 
            msd_internet_bytes_account = OCSAccount:create(username,account_type,next_30days_epoch, 100)
            return msd_internet_bytes_account:save()
        end
        msd_internet_bytes_account:add(100, next_30days_epoch)

        return msd_internet_bytes_account:save()

	end, {}, "infinity")
