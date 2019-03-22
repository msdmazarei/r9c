require "accounting/ocs_account"
return cel.ocs_account.transaction(
	function() 
	   print("setup called.")
		local a1 = OCSAccount:create("msd1","inetrnet_bytes",cel.utils.unixepoch_now()+1000000,100)
		a1:save()
		return a1
	end, {}, "infinity")
