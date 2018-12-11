defmodule GatewayCore.Drivers.GsmModemDriver.Output do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require Utilities
  @gateway_config Application.get_env(:gateway_core, __MODULE__)[node()]

  use GatewayCore.Outputs.Red9CobraSimpleOutGW
  # require IEx
  def nodes_to_run() do
    Logging.debug("Called, gateway_config:~p", [@gateway_config])
    r = @gateway_config[:nodes]
    Logging.debug("returned:~p", [r])
    r
  end

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

  def send_sms_to_gw(message = %DatabaseEngine.Models.SMS{receiver: receiver, body: body}) do
    pids =
      DynamicSupervisor.which_children(GatewayCore.GSMModemGateway.Supervisor)
      |> Enum.shuffle()
      |> Enum.map(fn x -> elem(x, 1) end)

    case pids do
      [] ->
        false

      _ ->
        gsm_modem = hd(pids)
        body = :unicode.characters_to_binary(body, :unicode, :utf16)
        r = :simple_gsm_modem_over_tcp.send_sms(gsm_modem, receiver, body)
        Logging.debug("Send SMS returned ~p", [r])

        case r do
          {:ok, _} -> true
          _ -> false
        end
    end
  end

  def send_sms_list(sms_list_to_send, state) do
    Logging.debug("Called.")
    results = sms_list_to_send |> Enum.map(&send_sms_to_gw/1)
    r = {state, results}
    Logging.debug("Retuens:~p", [r])
    r
  end

  def send_otp_list(otp_list, state) do
    Logging.debug("Called")
    results = otp_list |> Enum.map(fn _ -> false end)
    r = {state, results}
    Logging.debug("Returns:~p", [r])
    r
  end

  def send_charge_list(charge_list, state) do
    Logging.debug("Called")

    Logging.error(
      "NO CHARGE METHOD DEFINED FOR ~p MODULE, BUT SOMEONE ARE SENDING CHARGE !!! CHARGE LIST:~p",
      [__MODULE__, charge_list]
    )

    r = {state, charge_list |> Enum.map(fn _ -> false end)}
    Logging.debug("Returns:~p", [r])
    r
  end
end

defmodule GatewayCore.Drivers.GsmModemDriver.Input do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  #  alias DatabaseEngine.DurableQueue
  def receive_sms(nil, m) do
    Logging.debug("Called. with param: q_in:nil , message:~p", [m])
  end

  def receive_sms(q_in, m = {:normal_message, sender, body, arrive_epoch, _}) do
    Logging.debug("Called With Params: q_in:~p message:~p", [q_in, m])
    body = :unicode.characters_to_binary(body, :utf16, :unicode)
    Logging.debug("recieved body: ~p", [body])

    {receiver, body} =
      case String.split(body, "\n") do
        [] -> {"unknown", ""}
        [b] -> {"unknown", b}
        [r | remains] when length(remains) > 0 -> {r, remains |> Enum.join("\n")}
      end

    sms = %DatabaseEngine.Models.SMS{
      receiver: receiver,
      sender: sender,
      received_epoch: arrive_epoch,
      body: body,
      options: %{"gateway" => "gsm"}
    }

    Logging.debug("SMS to enqueu into q(~p) :~p ", [q_in, sms])
    sms = case sms do
      %DatabaseEngine.Models.SMS{} ->
      DatabaseEngine.Models.SMS.Helper.describe_stage(sms,__MODULE__,"ingress")
      _ -> sms
    end
    DatabaseEngine.DurableQueue.enqueue(q_in, sms)
  end

  def receive_sms(q_in, m) do
    Logging.debug("Called. with param: q_in:nil , message:~p --- UNKNOWN ", [q_in, m])
  end
end
