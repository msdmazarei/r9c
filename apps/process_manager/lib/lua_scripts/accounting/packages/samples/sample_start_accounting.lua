require "accounting/accounting_request"
acc_req = AccountingRequest:create(
    "nsm",
    "start",
    "mikrotik",
    {"fixed_internet"},
    {mac="00:00:01:02:03:04"},
    {creator="maspud"}
)

status,result = cel.kvdb.transaction(
    function (  )
        print("start processing")
        local s,r= acc_req:process()
        print("in tran s,r are:",s,r)
        return s,r
    end,{},"infinity"
)

print("processing finished result is" ,status, result)