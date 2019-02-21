# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :process_manager, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:process_manager, :key)
#
# You can also configure a 3rd-party app:
#
#     config :logger, level: :info
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env()}.exs"
config(:process_manager, ProcessManager.UnitProcess,
  wait_to_new_message_timeout_to_hibernate:
    System.get_env("UNIT_PROCESS_HIBERNATE_TIMEOUT") || 300_000,
  wait_to_new_message_timeout_to_terminate:
    System.get_env("UNIT_PROCESS_TERMINATE_TIMEOUT") || 600_000,
  success_Q: System.get_env("UNIT_PROCESS_SUCCESS_Q") || "unit_process_success_Q",
  fail_Q: System.get_env("USER_PROCESS_FAIL_Q") || "unit_process_fail_Q",
  cel_logging_Q: System.get_env("USER_PROCESS_CEL_LOGGING_Q") || "unit_process_logging_Q",
  cel_script_limitation: [run_timeout: 20_000, http_call_limit: 20]
)
