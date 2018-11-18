defmodule DatabaseEngine.Couchdb do
  require Logger
  @couchdb_props Application.get_env(:couchdb_connector, :props)
  @moduledoc """
    Couchdb database access apis and tools
  """

  @doc """
   tries to setup a database.
  """
  @spec db_setup :: :ok | :error | :conflict
  def db_setup do
    case Couchdb.Connector.Storage.storage_up(@couchdb_props) do
      {:ok, "{\"ok\":true}\n"} ->
        Logger.info(fn -> "Couchdb database storage created." end)
        :ok

      {:error, _} ->
        Logger.debug(fn -> "Couchdb database storage is already created." end)
        :conflict
    end
  end

  @doc """
   creates a new record in database with given data and idx.
  """

  @spec set(Map.t(), String.t()) :: String.t()
  def set(data, idx) do
    case Couchdb.Connector.create(@couchdb_props, data, idx) do
      {:ok, d} ->
        {:ok, d.payload}

      {:error, %{payload: %{"error" => "conflict", "reason" => "Document update conflict."}}} ->
        {:ok, %{"_rev" => rev}} = Couchdb.Connector.get(@couchdb_props, idx)

        case Couchdb.Connector.update(
               @couchdb_props,
               data |> Map.put_new("_id", idx) |> Map.put_new("_rev", rev)
             ) do
          {:ok, d} ->
            {:ok, d.payload}

          {:error, error} ->
            error
        end
    end
  end

  @doc """
    Reads a record by id
  """
  @spec get(String.t()) :: Map.t()
  def get(idx) do
    case Couchdb.Connector.get(@couchdb_props, idx) do
      {:ok, data} ->
        {:ok, data}

      {:error, %{"error" => error, "reason" => reason}} ->
        %{error: error, reason: reason}
    end
  end
end
