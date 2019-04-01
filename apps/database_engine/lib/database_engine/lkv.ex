defmodule DatabaseEngine.Interface.LKV do
  @moduledoc false
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @spec set(String.t(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
  @doc """
    set a new kv. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    pck = {LKVTb, k, v}

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
    Logging.debug("called. with key:~p",[key])
    opt = fn -> :mnesia.read(LKVTb, key) end
    opd = fn -> :mnesia.dirty_read(LKVTb, key) end

    op =
      case :mnesia.is_transaction() do
        true ->
          opt

        false ->
          opd
      end

      r = op.()
      Logging.debug("rtn pre value:~p",[r])
    case r do
      [{LKVTb, _, value}] ->
        Logging.debug("matched. returns: ~p",[value])
        value

      [] ->

        nil
    end
  end

  def get_for_update(key) do
    case :mnesia.is_transaction() do
      true ->
        case :mnesia.read(LKVTb, key, :write) do
          [{LKVTb, _, value}] ->
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
    opt = fn -> :mnesia.delete({LKVTb, key}) end
    opd = fn -> :mnesia.dirty_delete({LKVTb, key}) end

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
    oprt = fn -> :mnesia.read(LKVTb, key) end
    oprd = fn -> :mnesia.dirty_read(LKVTb, key) end
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
      [{LKVTb, _, value}] when is_integer(value) ->
        nv = value + by
        pck = {LKVTb, key, nv}
        opw.(pck)
        nv

      [{LKVTb, _, value}] when not is_integer(value) ->
        :error

      [] ->
        pck = {LKVTb, key, by}
        {opw.(pck), by}
        by
    end
  end
end
