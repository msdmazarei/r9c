defmodule Dispatcher.Process.VAS.UserProcess.Script do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require DatabaseEngine.Models.SMS

  alias Utilities.Logging
  alias :httpc, as: HTTPC
  alias :luerl, as: LUA

  @orig_message_key :orig_message_key


  def run_script(script_to_run, msg = %DatabaseEngine.Models.SMS{}, script_run_timeout \\ 5000) do
    main_process_id = self()
    ref = make_ref()

    pid =
      Process.spawn(
        fn ->
          try do

            lua_state = init_lua(msg)

            set_orig_message(msg, lua_state)
            {r, _} = LUA.do(script_to_run, lua_state)
            send(main_process_id, {:script, ref, :return, r})
          rescue
            e ->
              Logging.debug("Exception happen it is :~p",[e])
              send(main_process_id, {:script, ref, :error, e})
          end
        end,
        priority: :low
      )

    receive do
      {:script, m, status, r} when m == ref ->
        Logging.debug("script returned with state:~p and result:~p", [status, r])
    after
      script_run_timeout ->
        Logging.debug("script timeouted then kill it")
        Process.exit(pid, :kill)
    end
  end


  defp get_orig_message(_state) do
    Process.get(@orig_message_key)
  end

  defp set_orig_message(msg, _state) do
    Process.put(@orig_message_key, msg)
  end


  @spec http_request_limit(any) :: map()
  defp http_request_limit(_state) do
    %{"count" => 10}
  end

  @spec could_send_http_request(any()) :: boolean()
  defp could_send_http_request(state) do
    sent_items = Process.get("sent_http_request") || 0

    if sent_items > http_request_limit(state) do
      false
    else
      Process.put("sent_http_request", sent_items + 1)
      true
    end
  end

  def in_array(args, state) do
    Logging.debug("in_array_called with args:~p~n", [args])
    [item, list] = args
    result = list |> Enum.map(fn {_, x} -> x end) |> Enum.find(fn x -> x == item end) != nil
    {[result], state}
  end

  def is_subscribed(_args, state) do
    {[true], state}
  end

  def unsubscribe(_args, state) do
    :io.format("unsubscribe called in erl~n")
    {[true], state}
  end

  def reply(args, state) do
    Logging.debug("reply called. args:~p~n", [args])
    [msg_text | _] = args
    orig_msg = get_orig_message(state)

    msg_to_send = %DatabaseEngine.Models.SMS{
      orig_msg
      | receiver: orig_msg.sender,
        body: msg_text,
        sender: nil,
        options: Map.put(orig_msg.options, "reply_to_message", orig_msg)
    }

    gateway_to_send = GatewayCore.Utils.Helper.message_out_gateway(msg_to_send)
    Logging.debug("Gateway To Send Msg: ~p", [gateway_to_send])

    r =
      case Kernel.apply(gateway_to_send, :send, [msg_to_send]) do
        :ok -> true
        :nok -> false
      end

    Logging.debug("Gateway returned: ~p", [r])

    {[r], state}
  end

  def kvdb_get(args, state) do
    Logging.debug("kvdb_get called args:~p~n", [args])

    [key | _] = args
    result = DatabaseEngine.Interface.KV.get(key)
    Logging.debug("kvdb_get return: ~p", result)
    {[result], state}
  end

  def kvdb_set(args, state) do
    [k, v] = args
    Logging.debug("kvdb_set called args:~p~n", [args])
    result = DatabaseEngine.Interface.KV.set(k, v)
    Logging.debug("kvdb_set return:~p", [result])
    {[result], state}
  end

  def http_request_check(args, state) do
    if could_send_http_request(state) do
      http_request(args, state)
    else
      %{error: "exceeds http request count", status_code: -1}
    end
  end

  def http_request(args, state) do
    # http_request( method,headers,url,content-type,body )
    Logging.debug("http_request called args:~p~n", [args])
    [method, headers, url, content_type, body] = args

    method =
      case String.downcase(method) do
        "get" -> :get
        "post" -> :post
        "put" -> :put
        "patch" -> :patch
        "head" -> :head
      end

    url =
      if is_binary(url) do
        :binary.bin_to_list(url)
      else
        url
      end

    headers =
      case headers do
        nil -> []
        _ -> headers
      end

    req =
      case content_type do
        nil ->
          {url, headers}

        _ ->
          {url, headers, content_type, body}
      end

    resp =
      case HTTPC.request(method, req, [timeout: 5000], []) do
        {:error, reason} ->
          err = Logging.debug("httpc error: ~p", [reason])

          err =
            if is_binary(err) do
              err
            else
              to_string(err)
            end

          %{status_code: -1, error: err}

        {:ok, {status_code, body}} ->
          %{status_code: status_code, body: body}

        {:ok, {{_, status_code, _}, headers, body}} ->
          body =
            if is_binary(body) do
              body
            else
              :binary.list_to_bin(body)
            end

          %{status_code: status_code, headers: headers, body: body}
      end

    Logging.debug("request: ~p response: ~p ~n", [req, resp])

    {[Utilities.Conversion.nested_map_to_tuple_list(resp)], state}
  end


  defp init_lua(msg) do
    s0 = LUA.init()
    incoming_message = Utilities.Conversion.nested_map_to_tuple_list(msg)

    s1 =
      LUA.set_table(
        [:cel],
        Map.to_list(%{
          "incoming_message" => incoming_message,
          "is_subscribed" => &is_subscribed/2,
          "unsubscribe" => &unsubscribe/2,
          "in_array" => &in_array/2,
          "reply" => &reply/2,
          "service" =>
            Map.to_list(%{
              "unsub_keys" => ["a", "13", "43", "1234"]
            }),
          "kvdb" =>
            Map.to_list(%{
              "get" => &kvdb_get/2,
              "set" => &kvdb_set/2
            }),
          "http" => &http_request_check/2
        }),
        s0
      )
    s1
  end
end
