defmodule DatabaseEngine.Interface.KV do
  @moduledoc false

  @spec set(String.t(), any(), Integer.t()) :: {:atomic, :ok} | {:error, Atom.t()}
  @doc """
    set a new kv. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    :mnesia.transaction(fn ->
      pck = {KVTb, k, v}
      :ok = :mnesia.write(pck)
    end)
  end

  @spec get(String.t()) :: {:atomic, any()}
  @doc """
    gets value of a key from KV
  """
  def get(key) do
    :mnesia.transaction(fn ->
      case :mnesia.read(KVTb, key) do
        [{KVTb, _, value}] ->
          value

        [] ->
          nil
      end
    end)
  end

  @spec del(String.t()) :: {:atomic, :ok}
  @doc """
    deletes a key from KV database. always returns ```:OK```.
  """
  def del(key) do
    :mnesia.transaction(fn ->
      :mnesia.delete(KVTb, key)
    end)
  end

  @spec incr(String.t(), Integer.t()) :: {:atomic, Integer.t()} | {:atmoic, :error}
  @doc """
   Increases a value based on given key.
   Returrns ```0``` in case key is not yet created.
  """
  def incr(key, by \\ 1) do
    :mnesia.transaction(fn ->
      case :mnesia.read(KVTb, key) do
        [{KVTb, _, value}] when is_integer(value) ->
          nv = value + by
          pck = {KVTb, key, nv}
          :mnesia.write(pck)
          nv

        [{KVTb, _, value}] when not is_integer(value) ->
          :error

        [] ->
          pck = {KVTb, key, by}
          {:mnesia.write(pck), by}
          by
      end
    end)
  end
end
