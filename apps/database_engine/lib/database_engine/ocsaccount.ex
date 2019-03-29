defmodule DatabaseEngine.Interface.OCSAccount do
  @moduledoc false
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @spec set(String.t(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
  @doc """
    set a new kv. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    # Logging.debug("Called"  )
    pck = {OCSAccount, k, v}

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
    opt = fn -> :mnesia.read(OCSAccount, key) end
    opd = fn -> :mnesia.dirty_read(OCSAccount, key) end

    op =
      case :mnesia.is_transaction() do
        true ->
          opt

        false ->
          opd
      end

    case op.() do
      [{OCSAccount, _, value}] ->
        value

      [] ->
        nil
    end
  end

  def get_for_update(key) do
    case :mnesia.is_transaction() do
      true ->
        case :mnesia.read(OCSAccount, key, :write) do
          [{OCSAccount, _, value}] ->
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
    opt = fn -> :mnesia.delete({OCSAccount, key}) end
    opd = fn -> :mnesia.dirty_delete({OCSAccount, key}) end

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
    oprt = fn -> :mnesia.read(OCSAccount, key) end
    oprd = fn -> :mnesia.dirty_read(OCSAccount, key) end
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
      [{OCSAccount, _, value}] when is_integer(value) ->
        nv = value + by
        pck = {OCSAccount, key, nv}
        opw.(pck)
        nv

      [{OCSAccount, _, value}] when not is_integer(value) ->
        :error

      [] ->
        pck = {OCSAccount, key, by}
        {opw.(pck), by}
        by
    end
  end
end
