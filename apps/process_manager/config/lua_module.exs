use Mix.Config

config :process_manager, ProcessManager.Script,
  lua_modules_path: System.get_env("LUA_MODULE_PATH") || "/home/msd/projects/red9-cobra/apps/process_manager/lib/lua_scripts/?.lua;/home/msd/projects/red9-cobra/apps/process_manager/lib/lua_scripts/?/index.lua"

