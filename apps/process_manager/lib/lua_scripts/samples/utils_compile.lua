local code =
    [[ 
    require "utils"
    function gholi()
        loca a = { a= 1, b=2,c=3 }
        print(dump(a))
        print("this is ghili function inside")
    end
    
    ]]

local st0 = cel.utils.compile_code(code)
if st0 == false then
    print("failed to compile code")
else
    print("code successfully compiled")
    print("calling compiled function")

    cel.utils.call_function_in_state("gholi", {}, st0)
end
