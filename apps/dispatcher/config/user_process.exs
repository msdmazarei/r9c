use Mix.Config

config :dispatcher, Dispatcher.Process.VAS.UserProcess,
  wait_to_new_message_timeout_to_hibernate:
    System.get_env("USER_PROCESS_HIBERNATE_TIMEOUT") || 300_000,
  wait_to_new_message_timeout_to_terminate:
    System.get_env("USER_PROCESS_TERMINATE_TIMEOUT") || 600_000,
  success_Q: System.get_env("USER_PROCESS_SUCCESS_Q") || "user_process_success_Q",
  fail_Q: System.get_env("USER_PROCESS_FAIL_Q") || "user_process_fail_Q",
  cel_logging_Q: System.get_env("USER_PROCESS_CEL_LOGGING_Q") || "user_process_logging_Q",
  cel_script_limitation: [run_timeout: 20_000, http_call_limit: 20]
