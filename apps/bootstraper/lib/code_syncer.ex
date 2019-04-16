defmodule BootStraper.Code.GenServer do
  use GenServer
  require Logger

  require Utilities.Logging
  alias Utilities.Logging

  require Utilities.Admin.Node
  alias Utilities.Admin.Node, as: NodeAdmin
  @check_remote_node_code_tref "check_remote_node_code_tref"
  @r9c_lua_tar "r9c_lua.tar"

  @impl true
  def init(%{}) do
    Logging.debug("Called.")
    {:ok, check_remote_node_code_tref} = :timer.send_interval(15_000, :check_remote_node)

    {:ok,
     %{
       @check_remote_node_code_tref => check_remote_node_code_tref
     }}
  end

  @impl true
  def terminate(
        reason,
        _state = %{
          @check_remote_node_code_tref => check_remote_node_code_tref
        }
      ) do
    Logging.info("called. reason:~p", [reason])
    :timer.cancel(check_remote_node_code_tref)
    File.rm!(@r9c_lua_tar)
  end

  def is_remote_node_ok(nodename) do
    nodename =
      case nodename do
        v when is_binary(v) -> v |> String.to_atom()
        v when is_atom(v) -> v
      end

    need_to_sync =
      case NodeAdmin.get_all_loaded_modules(nodename) do
        {:badrpc, reason} ->
          Logging.error("problem in get all modules. node:~p reason:~p", [nodename, reason])
          true

        v ->
          app_modules = NodeAdmin.get_transferable_modules() |> Enum.map(fn {n, _} -> n end)
          remote_modules = v |> Enum.map(fn {n, _} -> n end)

          require_modules =
            app_modules
            |> Enum.filter(fn x ->
              !Enum.member?(remote_modules, x)
            end)

          # check require modules are exits
          code_which =
            require_modules
            |> Enum.map(fn x ->
              :rpc.block_call(nodename, :code, :which, [x], 1_000)
            end)

          badrpc_errors =
            code_which
            |> Enum.filter(fn x ->
              case x do
                {:badrpc, _} -> true
                _ -> false
              end
            end)

          case badrpc_errors do
            l when is_list(l) and length(l) > 0 ->
              Logging.error("badrpc on node:~p result:~p", [nodename, l])
              true

            _ ->
              if Enum.find(code_which, fn x -> x == :non_existing end) != nil do
                require_modules =
                  Enum.zip(code_which, require_modules)
                  |> Enum.filter(fn {x, _} ->
                    case x do
                      :non_existing -> true
                      _ -> false
                    end
                  end)
                  |> Enum.map(fn {_, x} -> x end)

                Logging.debug("require_modules length:~p", [length(require_modules)])
                length(require_modules) > 0
              else
                false
              end
          end
      end

    !need_to_sync
  end

  def do_sync(node) do
    if !is_remote_node_ok(node) do
      Logging.debug("remote node ~p should be synced", [node])
      mods = Utilities.Admin.Node.get_all_modules_bin()
      Utilities.Admin.Node.load_modules_to_target_node(node, mods)
      get_lua_tar()

      if File.exists?(@r9c_lua_tar) do
        ref = make_ref()

        case File.read(@r9c_lua_tar) do
          {:ok, bin} ->
            p = :erlang.spawn(node, Utilities.Admin.Node, :loop_to_wait_to_get_lua, [ref])
            send(p, {ref, bin})
            Logging.debug("message sent to process :~p", [p])

          e ->
            Logging.error("problem to read file :~p error:~p", [@r9c_lua_tar, e])
        end
      end
    else
      # Logging.debug("node:~p is synced, and no need to sync", [node])
      :ok
    end
  end

  @impl true
  def handle_info(:check_remote_node, state) do
    Utilities.all_active_nodes()
    |> Enum.map(fn x -> do_sync(x) end)

    {:noreply, state}
  end

  def get_lua_tar() do
    if File.exists?(@r9c_lua_tar) do
      :ok
    else
      {base_path, file_list} = get_lua_files_list()

      if base_path == nil do
        Logging.error("no LUA_LOAD_PATH var found.")
        :ok
      else
        :erl_tar.create(
          @r9c_lua_tar,
          file_list
          |> Enum.map(fn x ->
            base_path_len = String.length(base_path)
            total_len = String.length(x)
            tar_file_name = String.slice(x, base_path_len, total_len)
            {tar_file_name |> to_charlist(), x |> to_charlist()}
          end)
        )

        :ok
      end
    end
  end

  def get_lua_files_list() do
    case :os.get_env_var('LUA_LOAD_PATH') do
      false ->
        {nil, []}

      v ->
        {v |> Utilities.erl_list_to_iex_string(),
         create_files_list(v |> Utilities.erl_list_to_iex_string())}
    end
  end

  def create_files_list(path) do
    create_files_list(File.ls!(path), path)
    |> Enum.map(fn x ->
      Path.join(path, x)
    end)
  end

  def create_files_list(paths, path) do
    create_files_list(paths, path, path)
  end

  def create_files_list(paths, path, base_path) do
    Enum.reduce(paths, [], fn filename, acc ->
      filename_path = Path.join(path, filename)

      if File.dir?(filename_path) do
        acc ++ create_files_list(File.ls!(filename_path), filename_path, base_path)
      else
        filenm =
          if base_path,
            do: String.replace_leading(filename_path, base_path, ""),
            else: filename_path

        [filenm | acc]
      end
    end)
  end
end
