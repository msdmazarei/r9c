defmodule DatabaseEngine.Interface.LProcessData do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @table_name LProcessData
  def breif_processes_state() do
    all_process_states()
    |> Enum.map(fn {p, s} ->
      {p, s.process_name, s.processed_messages}
    end)
  end

  def all_process_states() do
    all()
    |> Enum.map(fn k ->
      pmodel = get(k)
      p = pmodel.local_pid

      case :rpc.call(p |> node, :sys, :get_state, [p], 1000) do
        {:error, _} -> nil
        {:badrpc, _} -> nil
        v -> {p, v}
      end

      # if :rpc.call(p |> node, :erlang, :is_process_alive, [p]) do
      #   {pmodel, :sys.get_state(pmodel.local_pid)}
      # else
      #   nil
      # end
    end)
    |> Enum.filter(fn y -> y != nil end)
  end

  def all() do
    case :mnesia.is_transaction() do
      true ->
        :mnesia.all_keys(@table_name)

      false ->
        :mnesia.dirty_all_keys(@table_name)
    end
  end

  def get_for_update(key) do
    case :mnesia.is_transaction() do
      true ->
        case :mnesia.read(@table_name, key, :write) do
          [{@table_name, _, value}] ->
            value

          _ ->
            nil
        end

      false ->
        Logging.error("try to read lock when we have no transaction context, key:~p", [key])
        throw("no transaction context to read_and_write_lock functionan")
    end
  end

  @spec set(String.t(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
  @doc """
    set a new process. Timeout is not implemented yet.
  """
  def set(k, v, _timeout \\ :infinity) do
    pck = {@table_name, k, v}

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
    opt = fn -> :mnesia.read(@table_name, key) end
    opd = fn -> :mnesia.dirty_read(@table_name, key) end

    op =
      case :mnesia.is_transaction() do
        true ->
          opt

        false ->
          opd
      end

    case op.() do
      [{@table_name, _, value}] ->
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
    opt = fn -> :mnesia.delete({@table_name, key}) end
    opd = fn -> :mnesia.dirty_delete({@table_name, key}) end

    case :mnesia.is_transaction() do
      true ->
        opt.()

      false ->
        opd.()
    end
  end
end
