defmodule GatewayCore.Inputs.Dummy do
  @gateway_config Application.get_env(:gateway_core, Red9Cobra.DUMMY)
  @ingress_q @gateway_config[:ingress_sms_Q]

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require DatabaseEngine.DurableQueue

  def ingress(sms = %DatabaseEngine.Models.SMS{options: options}) do
    sms_to_push = %DatabaseEngine.Models.SMS{sms | options: Map.put(options, "gateway", "DUMMY")}
    DatabaseEngine.DurableQueue.enqueue(@ingress_q, sms_to_push)
  end

  def get_sample_sms(sender, receiver, body) do
    %DatabaseEngine.Models.SMS{
      sender: sender,
      receiver: receiver,
      sms_center: "dummy",
      sent_epoch: Utilities.now(),
      received_epoch: Utilities.now(),
      body: body,
      id: UUID.uuid1()
    }
  end
end

defmodule GatewayCore.Outputs.Dummy do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  use GatewayCore.Outputs.Red9CobraSimpleOutGW

  @gateway_config Application.get_env(:gateway_core, Red9Cobra.DUMMY)
  @crash_p @gateway_config[:crash_probeblity]
  @fail_p @gateway_config[:send_failur_probeblity]

  def nodes_to_run() do
    @gateway_config[:nodes]
  end

  def gw_init() do
    Logging.debug("Called")
    []
  end

  def gw_queue_list() do
    Logging.debug("Called")
    Logging.debug("GC:~p", [@gateway_config])

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

        Logging.debug("crashp:~p CRASHC:~p fail_p:~p FAILC:~p", [
          crash_p,
          @crash_p,
          fail_p,
          @fail_p
        ])

        if crash_p < @crash_p do
          throw(:crashed)
        end

        if fail_p < @fail_p do
          Logging.warn("COULD NOT SEND SMS id:~p, receiver:~p, body:~p", [x.id, x.receiver,x.body])
          false
        else
          Logging.info("SEND SMS SUCCESSFULLY id:~p receiber:~p, body:~p", [x.id, x.receiver,x.body ])
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
