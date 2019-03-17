require "accounting/ocs_account"
return cel.ocs_account.transaction(
	function() 
		local a1 = OCSAccount:create("msd","inetrnet_bytes",19000000000,100)
		a1:save()
		return a1
	end, {}, "infinity")
