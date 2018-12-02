defmodule DatabaseEngine.Interface.Process do
  @moduledoc false

  @spec set(String.t(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
  @doc """
    set a new process. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    pck = {ProcessTb, k, v}

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
    gets value of a key from Process
  """
  def get(key) do
    opt = fn -> :mnesia.read(ProcessTb, key) end
    opd = fn -> :mnesia.dirty_read(ProcessTb, key) end

    op =
      case :mnesia.is_transaction() do
        true ->
          opt

        false ->
          opd
      end

    case op.() do
      [{ProcessTb, _, value}] ->
        value

      [] ->
        nil
    end
  end

  @spec del(String.t()) :: :ok
  @doc """
    deletes a key from Process database. always returns ```:OK```.
  """
  def del(key) do
    opt = fn -> :mnesia.delete({ProcessTb, key}) end
    opd = fn -> :mnesia.dirty_delete({ProcessTb, key}) end

    case :mnesia.is_transaction() do
      true ->
        opt.()

      false ->
        opd.()
    end
  end
end
