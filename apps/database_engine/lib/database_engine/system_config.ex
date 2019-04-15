defmodule DatabaseEngine.Interface.SystemConfig do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @table_name SystemConfigTb

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

  @spec set(any(), any(), Integer.t()) :: :ok | {:error, Atom.t()}
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

  @spec get(any()) :: any()
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

  @spec del(any()) :: :ok
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

  def do_transactionally(func) do
    case :mnesia.is_transaction() do
      true ->
        {:atomic, func.()}

      false ->
        :mnesia.transaction(func)
    end
  end

  def abort_transaction(reason) do
    :mnesia.abort(reason)
  end
end

defmodule DatabaseEngine.Interface.SystemConfig.NodeModel do
  defstruct node_name: "node_name",
            cookie: :nocookie,
            connected_nodes: [],
            display_name: "dispaly_name",
            is_code_master_node: false,
            is_config_node: false,
            is_dispatcher_node: false,
            is_process_node: false,
            version: 0,
            description: "",
            additional_props: %{}
end

defmodule DatabaseEngine.Interface.SystemConfig.Node.Repo do
  require DatabaseEngine.Interface.SystemConfig
  alias DatabaseEngine.Interface.SystemConfig
  require DatabaseEngine.Interface.SystemConfig.NodeModel
  alias DatabaseEngine.Interface.SystemConfig.NodeModel, as: Model

  def is_valid_model(model = %Model{}) do
    is_binary(model.node_name) and
      is_atom(model.cookie) and
      is_list(model.connected_nodes) and
      is_binary(model.display_name) and
      is_boolean(model.is_code_master_node) and
      is_boolean(model.is_config_node) and
      is_boolean(model.is_dispatcher_node) and
      is_boolean(model.is_process_node) and
      is_binary(model.description || "") and
      is_map(model.additional_props || %{}) and
      is_number(model.version)
  end

  def list_all_nodes() do
    nodes = SystemConfig.get("nodes") || []
    nodes |> Enum.map(fn x -> get_node(x) end)
  end

  @spec add_new_node(DatabaseEngine.Interface.SystemConfig.NodeModel.t()) ::
          {:aborted, :invalid_model | :already_node_exists}
          | {:atomic, DatabaseEngine.Interface.SystemConfig.NodeModel.t()}
  def add_new_node(model = %Model{}) do
    SystemConfig.do_transactionally(fn ->
      if is_valid_model(model) do
        key = {"node", model.node_name}
        node_config = SystemConfig.get_for_update(key)

        if node_config do
          SystemConfig.abort_transaction(:already_node_exists)
        else
          nodes = SystemConfig.get_for_update("nodes") || []
          nodes = [model.node_name | nodes]
          SystemConfig.set("nodes", nodes)
          SystemConfig.set(key, model)
          model
        end
      else
        SystemConfig.abort_transaction(:invalid_model)
      end
    end)
  end

  def edit_node(model = %Model{}) do
    SystemConfig.do_transactionally(fn ->
      if is_valid_model(model) do
        key = {"node", model.node_name}

        node_instance =
          SystemConfig.get_for_update(key) || SystemConfig.abort_transaction(:not_exist)

        if model.version == node_instance.version do
          model = %{model | version: model.version + 1}
          SystemConfig.set(key, model)
          model
        else
          SystemConfig.abort_transaction(:conflict)
        end
      else
        SystemConfig.abort_transaction({:error, :invalid_model})
      end
    end)
  end

  @spec del_node(binary()) :: {:aborted, :not_exists | any()} | {:atomic, Model.t()}
  def del_node(node_name) when is_binary(node_name) do
    SystemConfig.do_transactionally(fn ->
      key = {"node", node_name}
      n = SystemConfig.get(key)

      if n == nil do
        SystemConfig.abort_transaction(:not_exist)
      else
        SystemConfig.del(key)
        nodes = SystemConfig.get_for_update("nodes") || []
        nodes = nodes |> Enum.filter(fn x -> x != node_name end)
        SystemConfig.set("nodes", nodes)
        n
      end
    end)
  end

  def get_node(node_name) when is_binary(node_name) do
    key = {"node", node_name}
    SystemConfig.get(key)
  end
end
