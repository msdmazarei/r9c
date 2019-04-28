defmodule Utilities.Admin.Node do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def brief_node_info() do
    data = %{}
    {os_family, os_name} = :os.type()
    machine = %{}

    machine = machine |> Map.put("os", %{"family" => os_family, "name" => os_name})
    machine = machine |> Map.put("os_ver", :os.version() |> Tuple.to_list())

    machine =
      machine |> Map.put("logical_cpu_count", :erlang.system_info(:logical_processors_available))

    machine =
      machine
      |> Map.put(
        "cpu_usage",
        :cpu_sup.avg1() / 256 / :erlang.system_info(:logical_processors_available) * 100
      )

    machine =
      machine
      |> Map.put(
        "memory",
        :memsup.get_system_memory_data() |> Map.new()
      )

    data =
      data
      |> Map.put(
        "erl_ver",
        :erlang.system_info(:system_version) |> Utilities.erl_list_to_iex_string()
      )

    data = data |> Map.put("atom_count", :erlang.system_info(:atom_count))
    data = data |> Map.put("atom_limit", :erlang.system_info(:atom_limit))
    data = data |> Map.put("ets_count", :erlang.system_info(:ets_count))
    data = data |> Map.put("ets_limit", :erlang.system_info(:ets_limit))
    data = data |> Map.put("port_count", :erlang.system_info(:port_count))
    data = data |> Map.put("port_limit", :erlang.system_info(:port_limit))
    data = data |> Map.put("process_count", :erlang.system_info(:process_count))
    data = data |> Map.put("process_limit", :erlang.system_info(:process_limit))
    data = data |> Map.put("schedulers", :erlang.system_info(:schedulers))
    data = data |> Map.put("uptime_index", :erlang.statistics(:runtime) |> elem(0))
    data = data |> Map.put("cookie", :erlang.get_cookie())
    data = data |> Map.put("io", :erlang.statistics(:io) |> Tuple.to_list() |> Map.new())

    memory =
      :erlang.memory([
        :total,
        :processes,
        :processes_used,
        :system,
        :atom,
        :atom_used,
        :binary,
        :code,
        :ets
      ])
      |> Map.new()

    data = data |> Map.put("memory", memory)
    data = data |> Map.put("node_name", node())
    machine = machine |> Map.put("now", Utilities.now())

    rtn = data |> Map.put("machine", machine)
    rtn
  end

  def get_erlang_elixir_base_pathes() do
    {_, _, erlang_module_path} = :code.get_object_code(:erlang)
    {_, _, elixir_module_path} = :code.get_object_code(Enum)

    [erlang_module_path, elixir_module_path]
    |> Enum.map(fn x ->
      Path.dirname(x)
    end)
    |> Enum.map(fn x ->
      Path.join([x, "..", ".."])
    end)
    |> Enum.map(fn x -> Path.expand(x) end)
  end

  def get_transferable_modules() do
    erl_iex_paths = get_erlang_elixir_base_pathes()

    rtn =
      :code.all_loaded()
      |> Enum.filter(fn {module_name, module_path} ->
        r = module_name |> to_string()

        module_path =
          case module_path do
            :preloaded ->
              {_, _, m} = :code.get_object_code(module_name)
              m

            :cover_compiled ->
              {_, _, m} = :code.get_object_code(module_name)
              m

            _ ->
              module_path
          end

        module_path = module_path |> to_string() |> Path.expand()

        is_erliex_module =
          Enum.reduce(
            erl_iex_paths,
            false,
            fn s, acc ->
              is_it_in_base_path = module_path |> String.starts_with?(s)
              acc || is_it_in_base_path
            end
          )

        r = r |> String.ends_with?("MixProject") || r |> String.ends_with?("Mixfile")

        !(r || is_erliex_module)
      end)

    rtn
  end

  def get_all_modules_bin() do
    rtn =
      get_transferable_modules()
      |> Enum.map(fn {module_name, module_path} ->
        case :code.get_object_code(module_name) do
          :error ->
            Logging.error("problem to get object code for module:~p", [module_name])
            nil

          {_, b, _} when is_binary(b) ->
            {module_name, module_path, b}
        end
      end)

    all_is_done = rtn |> Enum.filter(fn x -> x == nil end)
    Logging.debug("all_is_dome:~p", [all_is_done])

    if all_is_done == [] do
      rtn
    else
      nil
    end
  end

  def load_modules_to_target_node(target_node, modules_list_to_load) do
    modules_list_to_load
    |> Enum.map(fn {n, _, _} ->
      :rpc.block_call(target_node, :code, :soft_purge, [n], 1_000)
    end)

    :timer.sleep(3_000)
    :rpc.block_call(target_node, :code, :atomic_load, [modules_list_to_load], 5_000)
  end

  def save_cookie_to_target_node(target_node, cookie_to_connect, cookie_to_save)
      when target_node != nil do
    target_node_atom =
      case is_atom(target_node) do
        true -> target_node
        _ -> target_node |> String.to_atom()
      end

    cookie_to_connect = cookie_to_connect |> String.to_atom()

    my_old_cookie = :erlang.get_cookie()

    is_connected =
      Utilities.all_active_nodes() |> Enum.filter(fn x -> x == target_node_atom end) |> length() >
        0

    if is_connected == false do
      :erlang.set_cookie(node(), cookie_to_connect)
      :net_kernel.connect_node(target_node_atom)
    end

    rtn =
      case :rpc.block_call(target_node_atom, :os, :getenv, ['HOME'], 5_000) do
        {:badrpc, reason} ->
          {:badrpc, reason}

        v when is_list(v) ->
          case :rpc.block_call(target_node_atom, Path, :join, [[v, '.erlang.cookie']], 5_000) do
            {:badrpc, reason} ->
              {:badrpc, reason}

            v ->
              :rpc.block_call(
                target_node_atom,
                System,
                :cmd,
                ["chmod", ["u+w", v]],
                5_000
              )

              case :rpc.block_call(
                     target_node_atom,
                     File,
                     :write,
                     [v, cookie_to_save |> to_string()],
                     5_000
                   ) do
                {:badrpc, reason} ->
                  {:badrpc, reason}

                {:error, reason} ->
                  {:error, reason}

                :ok ->
                  :ok
              end
          end
      end

    :erlang.set_cookie(node(), my_old_cookie)
    rtn
  end

  def get_all_loaded_modules(target_node) do
    target_node =
      case target_node do
        v when is_binary(v) -> v |> String.to_atom()
        a when is_atom(a) -> a
      end

    :rpc.block_call(target_node, :code, :all_loaded, [], 5000)
  end

  def loop_to_wait_to_get_lua(ref) do
    Logging.debug("called")

    receive do
      {^ref, lua_tar} ->
        Logging.debug("message arrived processs it")
        :erl_tar.extract({:binary, lua_tar})
        # set lua load path
        System.put_env("LUA_LOAD_PATH", File.cwd() |> elem(1))
    after
      120_000 ->
        Logging.debug("nothing arrived let exit")
    end
  end
end

defmodule Utilities.Admin.Mnesia do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def get_mnesia_info(node) when is_atom(node) do
    case :rpc.block_call(node, :mnesia, :system_info, [:all], 5000) do
      {:badrpc, reason} ->
        Logging.debug("badrpc:~p", [reason])
        {:badrpc, reason}

      v ->
        rtn =
          v
          |> Utilities.nested_tuple_to_list()
          |> Utilities.for_each_non_iterable_item(fn x ->
            if is_pid(x) do
              x |> :erlang.pid_to_list() |> Utilities.erl_list_to_iex_string()
            else
              x
            end
          end)
          |> Map.new()

        version = (rtn[:version] || "") |> Utilities.erl_list_to_iex_string()
        directory = (rtn[:directory] || "") |> Utilities.erl_list_to_iex_string()
        log_version = (rtn[:log_version] || "") |> Utilities.erl_list_to_iex_string()

        rtn
        |> Map.put(:version, version)
        |> Map.put(:directory, directory)
        |> Map.put(:log_version, log_version)
    end
  end

  def get_mnesia_table_info(node, tbname) when is_atom(node) and is_atom(tbname) do
    case :rpc.block_call(node, :mnesia, :table_info, [tbname, :all], 5000) do
      {:badrpc, reason} ->
        {:badrpc, reason}

      v ->
        v
        |> Enum.map(fn {x, y} ->
          if is_list(y) or is_map(y) or is_tuple(y) do
            {x, y |> Utilities.Conversion.nested_tuple_to_list()}
          else
            {x, y}
          end
        end)
        |> Map.new()
    end
  end

  def add_node_to_mnesia_cluster(node) when is_atom(node) do
    Logging.debug("called. node:~p", [node])

    case :rpc.block_call(node, :mnesia, :start, [], 5000) do
      {:badrpc, reason} ->
        Logging.error("error:~p", [reason])
        {:aborted, :io_lib.format("~p", [reason])}

      :ok ->
        Logging.debug("adding extra node:~p", [node])
        r = :mnesia.change_config(:extra_db_nodes, [node])
        Logging.debug("result:~p", [r])
        r
    end
  end

  def table_copy(node, table, type) when is_atom(node) and is_atom(table) and is_atom(type) do
    case :mnesia.add_table_copy(table, node, type) do
      {:atomic, :ok} ->
        {:atomic, :ok}

      {:aborted, {:already_exists, _, _}} ->
        nodes_of_requested_type = :mnesia.table_info(table, type)

        if nodes_of_requested_type |> Enum.member?(node) do
          {:atomic, :ok}
        else
          :mnesia.change_table_copy_type(table, node, type)
        end

      {:aborted, e} ->
        Logging.error("table_copy problem: ~p", [e])
        {:aborted, e}
    end
  end

  def add_ram_replica_node_for_table(node, tb) when is_atom(node) and is_atom(tb) do
    table_copy(node, tb, :ram_copies)
  end

  def add_disc_replica_node_for_table(node, tb) when is_atom(node) and is_atom(tb) do
    case get_mnesia_info(node) do
      {:badrpc, reason} ->
        {:aborted, reason}

      v ->
        if v[:use_dir] == false do
          :mnesia.change_table_copy_type(:schema, node, :disc_copies)
        end
    end

    table_copy(node, tb, :disc_copies)
  end

  def del_table_from_node(node, table) when is_atom(node) and is_atom(table) do
    r = :mnesia.del_table_copy(table, node)

    case get_mnesia_info(node) do
      {:badrpc, _} ->
        # {:aborted, reason}
        :ok

      v ->
        if length(v[:local_tables]) == 1 do
          :mnesia.change_table_copy_type(:schema, node, :ram_copies)
        end
    end

    r
  end

  def del_node(node) when is_atom(node) do
    case get_mnesia_info(node) do
      {:badrpc, reason} ->
        {:aborted, reason}

      v ->
        local_tables = v[:local_tables]

        r =
          local_tables
          |> Enum.filter(fn x -> x != :schema end)
          |> Enum.reduce_while(true, fn tb, _ ->
            Logging.debug("deleting node:~p table:~p", [node, tb])

            case del_table_from_node(node, tb) do
              {:atomic, :ok} -> {:cont, true}
              e -> {:halt, e}
            end
          end)

        if r == true do
          case :rpc.block_call(node, :mnesia, :stop, [], 5000) do
            {:badrpc, reason} ->
              Logging.error("problem to stop mnesia:~p", [reason])
              {:aborted, reason}

            :stopped ->
              del_table_from_node(node, :schema)
          end
        else
          {:aborted, r}
        end
    end
  end
end
