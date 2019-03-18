package.path = package.path .. ";/home/msd/projects/red9-cobra/apps/process_manager/lib/lua_scripts/?.lua;/home/msd/projects/red9-cobra/apps/process_manager/lib/lua_scripts/?/index.lua"
require "utils/list"
require "utils"
local l = {1,2,3,4,5}
local r =  list_filter(function(i) 
	if i==2 then
		return true
	else
		return false
	end
end	, l)
print(dump(l))
print(dump(r))