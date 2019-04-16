defmodule BootStraper.Mnesia.GServer do
  use GenServer

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def join_node_to_db_cluster(node) do
    Logging.debug("called. node:~p", [node])

    case :mnesia.system_info(:db_nodes) |> Enum.member?(node) do
      true ->
        Logging.debug("~p is already member of cluster", [node])
        true

      false ->
        Logging.debug("~p is not part of mnesia cluster. let join it.", [node])

        case Utilities.Admin.Mnesia.add_node_to_mnesia_cluster(node) do
          {:ok, _} ->
            true

          {:error, reason} ->
            Logging.error("problem to join node:~p to cluster cause of:~p", [
              node,
              reason
            ])

            false
        end
    end
  end

  def add_local_tables_to_node(node) do
    local_tables = [LProcessData, LProcess, LKVTb]

    case Utilities.Admin.Mnesia.get_mnesia_info(node) do
      {:badrpc, reason} ->
        Logging.error("problem to retrive mnesia info of node:~p", [node])
        false

      v ->
        tables_need_to_transfer =
          local_tables
          |> Enum.filter(fn x ->
            r = v[:local_tables] |> Enum.member?(x)
            !r
          end)

        Logging.debug("tables need to transfer:~p", [tables_need_to_transfer])

        tables_need_to_transfer
        |> Enum.map(fn x ->
          Utilities.Admin.Mnesia.add_ram_replica_node_for_table(node, x)
        end)
    end
  end

  def sysconfig_table_to_target_node(node) do
    config_tables = [SystemConfigTb]
    Logging.debug("check node is config node:~p", [node])
    n = DatabaseEngine.Interface.SystemConfig.Node.Repo.get_node(node |> to_string())
    target_node_mnesia_info = Utilities.Admin.Mnesia.get_mnesia_info(node)

    case target_node_mnesia_info do
      {:badrpc, reason} ->
        {:badrpc, reason}

      mnesia_info ->
        if n == nil do
          Logging.debug("there is no config for node name :~p", [node])
          true
        else
          if n.is_config_node == true do
            Logging.debug("node:~p is config node", [node])

            config_tables
            |> Enum.map(fn x ->
              Utilities.Admin.Mnesia.add_disc_replica_node_for_table(node, x)
            end)
          else
            Logging.debug("node:~p is not config node.", [node])

            tables_to_delete =
              config_tables
              |> Enum.filter(fn x ->
                mnesia_info[:local_tables] |> Enum.member?(x)
              end)

            Logging.debug("tables to remove:~p", [tables_to_delete])

            tables_to_delete
            |> Enum.map(fn x ->
              r = Utilities.Admin.Mnesia.del_table_from_node(node, x)
              Logging.debug("removing table:~p result:~p", [x, r])
              r
            end)
          end
        end
    end
  end

  def dispatcher_table_check(node) do
    dispatcher_tables = [ProcessTb]
    Logging.debug("check node is dispatcher node:~p", [node])
    n = DatabaseEngine.Interface.SystemConfig.Node.Repo.get_node(node |> to_string())
    target_node_mnesia_info = Utilities.Admin.Mnesia.get_mnesia_info(node)

    case target_node_mnesia_info do
      {:badrpc, reason} ->
        {:badrpc, reason}

      mnesia_info ->
        if n == nil do
          Logging.debug("there is no dispatcher for node name :~p", [node])
          true
        else
          if n.is_dispatcher_node == true do
            Logging.debug("node:~p is dispatcher node", [node])

            dispatcher_tables
            |> Enum.map(fn x ->
              Utilities.Admin.Mnesia.add_disc_replica_node_for_table(node, x)
            end)
          else
            Logging.debug("node:~p is not config node.", [node])

            tables_to_delete =
              dispatcher_tables
              |> Enum.filter(fn x ->
                mnesia_info[:local_tables] |> Enum.member?(x)
              end)

            Logging.debug("tables to remove:~p", [tables_to_delete])

            tables_to_delete
            |> Enum.map(fn x ->
              r = Utilities.Admin.Mnesia.del_table_from_node(node, x)
              Logging.debug("removing table:~p result:~p", [x, r])
              r
            end)
          end
        end
    end
  end

  def init(args) do
    :ok = :net_kernel.monitor_nodes(true)
    {:ok, %{}}
  end

  def handle_info({:nodeup, nodename}, state) do
    Logging.info("node:~p is up", [nodename])

    spawn(fn ->
      Logging.debug("wait 3 seconds and allow target node to prepare itself")
      :timer.sleep(3_000)
      join_node_to_db_cluster(nodename)
      add_local_tables_to_node(nodename)
      sysconfig_table_to_target_node(nodename)
      dispatcher_table_check(nodename)
    end)

    {:noreply, state}
  end

  def handle_info({:nodedown, nodename}, state) do
    Logging.info("node:~p is down", [nodename])
    {:noreply, state}
  end
end
