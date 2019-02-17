defmodule RadiusServer do
  @behaviour :radius
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def hello() do
    :radius.start_link(__MODULE__, 1812)
  end

  def init(address, port) do
    Logging.debug("Called. address: ~p port: ~p", [address, port])
    {:ok, []}
  end

  def request(address, port, packet, state) do
    Logging.debug("request called params , addrss:~p port: ~p satat: ~p", [address, port, state])
    request(packet, "testing123")
  end

  def request(packet, secret) do
    Logging.debug("request/2 called. ")
    Logging.debug("packet: ~p", [:radius.codec(packet)])
  end

  def terminate(reason, state) do
    Logging.debug("terminate called. reason: ~p state: ~p", [reason, state])
  end
end
