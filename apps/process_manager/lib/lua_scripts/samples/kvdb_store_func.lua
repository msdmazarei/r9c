function t()
    print("this is funcion t")
end

cel.kvdb.set("t_func",t)




m = cel.kvdb.kvdb_func_pass()
print("done", type(m))
m()

r=cel.kvdb.get("t_func")
print("r retrieved......",r)
_G[r]()
return "ok"




r=cel.kvdb.get("t_func")
print("r retrieved......",type(r))
r()
_G[r]()
return "ok"



--- properly worked

t = {
    f1 = function()
        print("this is f1 func")
    end,
    f2 = function()
        print("this is f2 func")
    end,
    f3 = {
        f3_1 = function()
            print("f3_1 is called.")
        end, 
        f3_2 = {
            f3_2_1 = 1,
            f3_2_2 = function(a,b,c)
                print("a:",a)
                print("b;",b)
                print("c:",c)
                print("f3_2_2 is called.")
            end
        }
    }

}
cel.kvdb.set("t_1",t)


r=cel.kvdb.get("t_1")
print("t1:",type(r["f1"]))
print("f_3_1",type(r["f3"]["f3_1"]))
r["f3"]["f3_2"]["f3_2_2"](1,"salam","masoud")