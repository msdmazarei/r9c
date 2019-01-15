defmodule DatabaseEngine.Mnesia.Records do
  require Record

  Record.defrecord(:client_tb, ClientTb,
    idx: nil,
    create_unixepochx: nil,
    version: nil,
    name: nil,
    _internal: nil
  )

end
