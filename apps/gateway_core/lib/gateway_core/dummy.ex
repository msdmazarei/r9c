defmodule GatewayCore.Outputs.Dummy do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @gateway_config Application.get_env(:gateway_core, Red9Cobra.DUMMY)
  @crash_p @gateway_config[:crash_probeblity]
  @fail_p @gateway_config[:send_failur_probeblity]

  def gw_init() do
    Logging.debug("Called")
    []
  end

  def gw_queue_list() do
    Logging.debug("Called")

    r = [
      in_Q: @gateway_config[:input_Q],
      success_Q: @gateway_config[:success_Q],
      fail_Q: @gateway_config[:fail_Q]
    ]

    Logging.debug("Returns:~p", [r])
    r
  end

  def send_sms_list(sms_list_to_send, state) do
    Logging.debug("Called.")

    results =
      sms_list_to_send
      |> Enum.map(fn x ->
        crash_p = :rand.uniform()
        fail_p = :rand.uniform()

        if crash_p > @crash_p do
          throw(:crashed)
        end

        if fail_p > @fail_p do
          Logging.warn("COULD NOT SEND SMS id:~p", [x.id])
          false
        else
          Logging.info("SEND SMS SUCCESSFULLY id:~p", [x.id])
        end
      end)

    r = {state, results}
    Logging.debug("Retuens:~p", [r])
    r
  end

  def send_otp_list(otp_list, state) do
    Utilities.randseed()
    Logging.debug("Called")

    results =
      otp_list
      |> Enum.map(fn x ->
        crash_p = :rand.uniform()
        fail_p = :rand.uniform()

        if crash_p > @crash_p do
          throw(:crashed)
        end

        if fail_p > @fail_p do
          Logging.warn("COULD NOT SEND OTP id:~p", [x.id])
          false
        else
          Logging.info("SEND OTP SUCCESSFULLY id:~p", [x.id])
        end
      end)

    r = {state, results}
    Logging.debug("Returns:~p", [r])
    r
  end

  def send_charge_list(charge_list, state) do
    results =
      charge_list
      |> Enum.map(fn x ->
        crash_p = :rand.uniform()
        fail_p = :rand.uniform()

        if crash_p > @crash_p do
          throw(:crashed)
        end

        if fail_p > @fail_p do
          Logging.warn("COULD NOT SEND CHARGE id:~p", [x.id])
          false
        else
          Logging.info("SEND CHARGE SUCCESSFULLY id:~p", [x.id])
        end
      end)

    r = {state, results}
    Logging.debug("Returns:~p", [r])
    r
  end
end
