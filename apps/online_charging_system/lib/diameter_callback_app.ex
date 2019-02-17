defmodule OnlineChargingSystem.Servers.DiameterCallbackApp do
  @moduledoc false

  @behaviour :diameter_app

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def peer_up(svc_name, peer, state) do
    Logging.debug("Called. s:~p p:~p s:~p", [svc_name, peer, state])
    state
  end

  def peer_down(svc_name, peer, state) do
    Logging.debug("Called. s:~p p:~p s:~p", [svc_name, peer, state])
    state
  end

  def pick_peer(localCandidates, remoteCandidates, svcName, state) do
    Logging.debug("Called. l:~p r:~p sn:~p s:~p", [
      localCandidates,
      remoteCandidates,
      svcName,
      state
    ])

    if localCandidates
       |> length > 0 do
      p =
        localCandidates
        |> hd()

      {:ok, p}
    else
      false
    end
  end

  def prepare_request(packet, svcName, peer) do
    Logging.debug("Called. pak:~p svc:~p peer:~p", [packet, svcName, peer])
    {:send, packet}
  end

  def prepare_retransmit(packet, svcName, peer) do
    Logging.debug("Called. pack:~p s:~p p:~p", [packet, svcName, peer])
    {:send, packet}
  end

  def handle_answer(packet, request, svcName, peer) do
    Logging.debug("Called. pack:~p r:~p s:~p p:~p", [packet, request, svcName, peer])
    :ok
  end

  def handle_error(reason, request, svcName, peer) do
    Logging.debug("Called. reasin:~p req:~p s:~p p:~p", [reason, request, svcName, peer])
    :ok
  end

  def handle_request(packet, svcName, peer) do
    :io.fwrite("handle_request called.")
    Logging.debug("Called. p:~p s:~p p:~p", [packet, svcName, peer])
    {:answer_message, 3000}
  end
end
