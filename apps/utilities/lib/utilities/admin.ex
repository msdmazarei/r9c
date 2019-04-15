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
end
