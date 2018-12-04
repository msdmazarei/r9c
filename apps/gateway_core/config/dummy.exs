use Mix.Config

config :gateway_core, Red9Cobra.DUMMY,
  name: "DUMMY",
  input_Q: System.get_env("DUMMY_IN_Q") || "dummy_input_q",
  success_Q: System.get_env("DUMMY_SUCCESS_Q") || "dummy_success_q",
  fail_Q: System.get_env("DUMMY_FAIL_Q") || "dummy_fail_q",
  crash_probeblity: System.get_env("DUMMY_CRASH_PROBEBLITY") || 0.6,
  send_failur_probeblity: System.get_env("DUMMY_SEND_PROBEBLITY") || 0.1,
  ingress_sms_Q: System.get_env("DUMMY_INGRESS_Q") || "in_dummy",
  nodes: [:"n2@s1.kafka.local"]
