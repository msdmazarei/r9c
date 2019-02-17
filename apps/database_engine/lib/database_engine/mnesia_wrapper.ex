defmodule DatabaseEngine.MnesiaWrapper do
  def normalize_return_value(r) do
    case r do
      :ok -> true
      {:ok, res} -> {true, res}
      {:aborted, err} -> {false, err}
      {:atomic, res} -> {true, res}
      v -> v
    end
  end

  def write(record) do
    r =
      if :mnesia.is_transaction() do
        :mnesia.s_write(record)
      else
        :mnesia.dirty_write(record)
      end

    normalize_return_value(r)
  end

  def do_transactional(func) do
    r = :mnesia.transaction(func, 1)
    normalize_return_value(r)
  end

  def read(tab, key) do
    r =
      if :mnesia.is_transaction() do
        :mnesia.read(tab, key)
      else
        :mnesia.dirty_read(tab, key)
      end

    normalize_return_value(r)
  end

  def delete(tab, key) do
    r =
      if :mnesia.is_transaction() do
        :mnesia.delete(tab, key, :write)
      else
        :mnesia.dirty_delete(tab, key)
      end

    normalize_return_value(r)
  end
end

defprotocol DatabaseEngine.MnesiaWrapper.Serializer do
  @spec to_mnesia_record(any()) :: any()
  def to_mnesia_record(data)
end
