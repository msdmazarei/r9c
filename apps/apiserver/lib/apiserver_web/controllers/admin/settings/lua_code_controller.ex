defmodule ApiserverWeb.Admin.Settings.Code.LUA.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities

  require DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher
  alias DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher, as: Model

  require DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher.Repo
  alias DatabaseEngine.Interface.SystemConfig.KafkaQueueDispatcher.Repo

  @api_duration "api_duration"
  def params_to_struct(params) do
    Utilities.to_struct(Model, params)
  end

  def response_body(st_time, result) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => result
    }

    rtn
  end

  def get_lua_module_path() do
    lua_modules_path =
      case :os.get_env_var('LUA_LOAD_PATH') do
        false ->
          "."

        v ->
          v
      end

    lua_modules_path = Path.expand(lua_modules_path)
    lua_modules_path
  end

  def execute_lua(conn, params) do
    Logging.debug("Called. params:~p", [params])
    st_time = Utilities.now()
    code_to_run = params["code"] || ""
    in_msg = params["msg"] || %{}
    user_process_state = params["state"] || %{}
    timeout = params["timeout"] || 5000

    {status, rtn} =
      case ProcessManager.Script.run_script(
             code_to_run,
             in_msg,
             user_process_state,
             %{},
             timeout,
             true,
             true
           ) do
        {:error, e} ->
          {500, Utilities.to_string(e)}

        {:return, value, %{"virtual_console" => vc}} ->
          {200,
           %{
             "result" => value,
             "virtual_console" => vc
           }}
      end

    json_result = response_body(st_time, rtn)
    conn |> send_response(status, json_result)
  end

  def list_all_modules(conn, _params) do
    Logging.debug("called.")
    st_time = Utilities.now()

    lua_modules_path = get_lua_module_path()
    lua_modules_path_len = String.length(lua_modules_path)

    file_lists =
      Utilities.FileExt.ls_r(lua_modules_path)
      |> Enum.filter(fn x ->
        String.slice(x, String.length(x) - 3, 3) == "lua"
      end)
      |> Enum.map(fn x ->
        r = String.slice(x, lua_modules_path_len, String.length(x))

        if String.starts_with?(r, "/") do
          String.slice(r, 1, String.length(r))
        else
          r
        end
      end)

    body = response_body(st_time, file_lists)
    conn |> send_response(200, body)
  end

  def get_module(conn, params) do
    Logging.debug("called params:~p", [params])
    st_time = Utilities.now()
    lua_modules_path = get_lua_module_path()
    module_name = params["module_name"] || ""

    target_module_path = Path.join([lua_modules_path, module_name])

    {status, result} =
      case File.exists?(target_module_path) do
        false ->
          {404, nil}

        true ->
          case File.read(target_module_path) do
            {:ok, content} -> {200, content}
            {:error, err} -> {500, Utilities.to_string(err)}
          end
      end

    body = response_body(st_time, result)
    conn |> send_response(status, body)
  end

  def create_module(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()
    module_name = params["module_name"]
    module_content = params["code"]
    lua_modules_path = get_lua_module_path()
    target_module_path = Path.join([lua_modules_path, module_name]) |> Path.expand()
    dir_part = Path.dirname(target_module_path)

    {status, result} =
      case File.mkdir_p(dir_part) do
        :ok ->
          case File.write(target_module_path, module_content) do
            :ok -> {204, nil}
            {:error, e} -> {500, e}
          end

        {:error, err} ->
          {500, Utilities.to_string(err)}
      end

    body = response_body(st_time, result)
    conn |> send_response(status, body)
  end

  def del_module(conn, params) do
    Logging.debug("called. params:~p", [params])
    st_time = Utilities.now()
    lua_modules_path = get_lua_module_path()
    target_module = params["module_name"]
    target_module_path = Path.join([lua_modules_path, target_module]) |> Path.expand()

    {status, result} =
      case File.rm(target_module_path) do
        :ok -> {204, nil}
        {:error, e} -> {500, Utilities.to_string(e)}
      end

    body = response_body(st_time, result)
    conn |> send_response(status, body)
  end
end
