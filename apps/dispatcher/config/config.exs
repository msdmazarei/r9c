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
#     config :dispatcher, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:dispatcher, :key)
#
# You can also configure a 3rd-party app:
#
config :logger, level: :debug
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env()}.exs"
import_config "user_process.exs"

config :dispatcher,
  input_queues: [
    "n2@s1.kafka.local": ["in_dummy", "gsm_ingress"],
    "n1@s1.kafka.local": ["diameter_queue"],
    "n3@s1.kafka.local": ["in_imi", "in_irmtn", "radius_input_q", ]
  ]

config :dispatcher, Dispatcher.Process,
  # maximum time to wait for aliveness response of process
  alive_response_for_UP: System.get_env("DISPATCHER_UP_ALIVE_TIMEOUT") || 1_000,
  # maximum time to wait to get request process resoinse
  maximum_wait_time_for_UP_responses: System.get_env("DISPATCHER_UP_RESP_MAX_TIMEOUT") || 7_000,
  user_process_timeout: System.get_env("DISPATCHER_UP_LOCAL_CALL_TIMEOUT") || 5_000,
  process_creation_timeout: System.get_env("DISPATCHER_UP_CREATION_TIMEOUT") || 1_000,
  dispatcher_fail_Q: System.get_env("DISPATCHER_FAIL_Q") || "dispatcher_fail_Q"
