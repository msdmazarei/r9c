require "accounting/accounting_request"
acc_req = AccountingRequest:create(
    "user_which_no_exist",
    "start",
    "mikrotik",
    {"fixed_internet"},
    {mac="00:00:01:02:03:04"},
    {creator="maspud"}
)

status,result = cel.kvdb.transaction(
    function (  )
        return acc_req:process()
    end,{},"infinity"
)