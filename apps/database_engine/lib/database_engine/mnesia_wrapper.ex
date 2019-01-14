defmodule DatabaseEngine.MnesiaWrapper do

  def write(record) do
    if :mnesia.is_transaction() do
      :mnesia.s_write(record)
    else
      :mnesia.dirty_write(record)
    end
  end


  def read(tab,key) do
    if :mnesia.is_transaction() do
      :mnesia.read(tab,key)
    else
      :mnesia.dirty_read(tab,key)
    end
  end

end

defprotocol DatabaseEngine.MnesiaWrapper.Serializer do
  @spec to_mnesia_record(any()) :: any()
  def to_mnesia_record(data)
end

