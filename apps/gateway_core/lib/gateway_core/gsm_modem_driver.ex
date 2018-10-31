defmodule GatewayCore.Drivers.GsmModemDriver.Output do
  @moduledoc false

  use KafkaEx.GenConsumer

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  alias DatabaseEngine.DurableQueue

  defmodule State do
    defstruct partition: 0, topic: ""
  end

  def init(_topic, _partition) do
    {:ok, %State {partition: _partition, topic: _topic}}
  end


  def send_sms(message = %DatabaseEngine.Models.SMS{receiver: receiver, body: body}) do
    Logging.debug("Called With Params: message:~p", [message])
    pids = DynamicSupervisor.which_children(GatewayCore.GSMModemGateway.Supervisor)
           |> Enum.shuffle
           |> Enum.map(fn x -> elem(x, 1) end)

    gsm_modem=hd(pids)
    body = :unicode.characters_to_binary(body,:unicode,:utf16)
    :simple_gsm_modem_over_tcp.send_sms(gsm_modem,receiver,body)
  end

  @impl true
  def handle_message_set(message_set, state = %State{}) do

    for %Message{value: message} <- message_set do
      deserialized_message = message
                             |> DurableQueue.deserialize()
      Logging.debug("Message:#{message} Deserialized: ~p ", [deserialized_message])
      deserialized_message = Map.take(deserialized_message,["receiver","body"])
      deserialized_message  = for {key, val} <- deserialized_message, into: %{}, do: {String.to_atom(key), val}
      msg = struct(%DatabaseEngine.Models.SMS{}, deserialized_message)
      send_sms(msg)
    end
    {:async_commit, state}
  end


end
