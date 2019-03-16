defmodule DatabaseEngine.Interface.KV do
  @moduledoc false
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @spec set(String.t(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
  @doc """
    set a new kv. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    pck = {KVTb, k, v}

    opt = fn ->
      :ok = :mnesia.write(pck)
    end

    opd = fn ->
      :ok = :mnesia.dirty_write(pck)
    end

    case :mnesia.is_transaction() do
      false ->
        opd.()

      true ->
        opt.()
    end
  end

  @spec get(String.t()) :: any()
  @doc """
    gets value of a key from KV
  """
  def get(key) do
    opt = fn -> :mnesia.read(KVTb, key) end
    opd = fn -> :mnesia.dirty_read(KVTb, key) end

    op =
      case :mnesia.is_transaction() do
        true ->
          opt

        false ->
          opd
      end

    case op.() do
      [{KVTb, _, value}] ->
        value

      [] ->
        nil
    end
  end

  def get_for_update(key) do
    case :mnesia.is_transaction() do
      true ->
        case :mnesia.read(KVTb, key, :write) do
          [{KVTb, _, value}] ->
            value

          _ ->
            nil
        end

      false ->
        Logging.error("try to read lock when we have no transaction context, key:~p", [key])
        throw("no transaction context to read_and_write_lock functionan")
    end
  end

  @spec del(String.t()) :: :ok
  @doc """
    deletes a key from KV database. always returns ```:OK```.
  """
  def del(key) do
    opt = fn -> :mnesia.delete({KVTb, key}) end
    opd = fn -> :mnesia.dirty_delete({KVTb, key}) end

    case :mnesia.is_transaction() do
      true ->
        opt.()

      false ->
        opd.()
    end
  end

  @spec incr(String.t(), Integer.t()) :: Integer.t() | :error
  @doc """
   Increases a value based on given key.
   Returrns ```0``` in case key is not yet created.
  """
  def incr(key, by \\ 1) do
    oprt = fn -> :mnesia.read(KVTb, key) end
    oprd = fn -> :mnesia.dirty_read(KVTb, key) end
    opwt = fn pck -> :mnesia.write(pck) end
    opwd = fn pck -> :mnesia.dirty_write(pck) end

    {opr, opw} =
      case :mnesia.is_transaction() do
        true ->
          {oprt, opwt}

        false ->
          {oprd, opwd}
      end

    case opr.() do
      [{KVTb, _, value}] when is_integer(value) ->
        nv = value + by
        pck = {KVTb, key, nv}
        opw.(pck)
        nv

      [{KVTb, _, value}] when not is_integer(value) ->
        :error

      [] ->
        pck = {KVTb, key, by}
        {opw.(pck), by}
        by
    end
  end
end
