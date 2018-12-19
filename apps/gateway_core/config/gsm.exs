use Mix.Config

config :gateway_core, GatewayCore.Drivers.GsmModemDriver.Output,
  name: "GSM",
  "n2@s1.kafka.local": [
    input_Q: System.get_env("GSM_IN_Q") || "gsm_input_q",
    success_Q: System.get_env("GSM_SUCCESS_Q") || "gsm_success_q",
    fail_Q: System.get_env("GSM_FAIL_Q") || "gsm_fail_q",
    modems: [{"127.0.0.1", 3285}],
    ingress_Q: System.get_env("GSM_INGRESS_Q") || "gsm_ingress",
    nodes: [:"n1@s1.kafka.local"],
    throttle: [{2, 5_000}, {10, 60_000}]
  ]
