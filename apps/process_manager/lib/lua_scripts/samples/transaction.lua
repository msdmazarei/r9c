

success, result = cel.kvdb.transaction(
    function (  )
        local r = cel.kvdb.set("msd_hello","salam1")
        return false, "problem in commiting"
    end,
    {},
    "infinity"
)
print("success",success)
print("result", result)



success, result = cel.kvdb.transaction(
    function (  )
        local r = cel.kvdb.set("msd_hello","salam2")
        return true, "done"
    end,
    {},
    "infinity"
)
print("success",success)
print("result", result)
