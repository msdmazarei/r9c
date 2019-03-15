require "diameter/diameter_avp"
require "diameter/diameter_general_avp"

local a = general_uint32_avp(12,10)
print (a:test_cel_function())
return 0 
