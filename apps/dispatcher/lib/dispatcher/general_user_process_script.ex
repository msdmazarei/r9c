defmodule Dispatcher.UserProcess.Script do
  @moduledoc false

  require Logger
  require Utilities.Logging
  require GatewayCore.Utils.Helper

  alias Utilities.Logging
  alias :httpc, as: HTTPC
  alias :luerl, as: LUA

  @orig_message_key :orig_message_key
  @spec run_script(String.t(), any(), any(), integer()) ::
          {:return, any()} | {:error, any()}
  @spec run_script(any(), any(), any(), any(), :infinity | non_neg_integer()) :: {any(), any()}
  def parse_rtn_value(r, lstate) do
    r
    |> Enum.map(fn x ->
      case x do
        {:tref, _} -> LUA.decode(x, lstate)
        _ -> x
      end
    end)
  end

  def to_lua(var) when is_number(var) or is_binary(var) do
    var
  end

  def to_lua(var) do
    v1 = Utilities.nested_tuple_to_list(var)
    v2 = Utilities.Conversion.nested_map_to_tuple_list(v1)
    v2
  end

  def to_elixir(var) when is_list(var) do
    if Utilities.is_list_of_tuples(var) do
      v1 =
        var
        |> Enum.map(fn {k, v} ->
          {k, to_elixir(v)}
        end)

      Map.new(v1)
    else
      var
      |> Enum.map(fn x ->
        to_elixir(x)
      end)
    end
  end

  def to_elixir(var) when is_number(var) or is_bitstring(var) do
    var
  end

  def run_script(
        script_to_run,
        msg,
        user_process_state,
        additional_functionality \\ %{},
        script_run_timeout \\ 5000
      ) do
    Logging.debug("Called.")
    main_process_id = self()
    ref = make_ref()

    pid =
      Process.spawn(
        fn ->
          try do
            lua_state = init_lua(msg, additional_functionality)

            set_orig_message(msg, lua_state)
            set_user_process_state(user_process_state)

            {r, lstate} = LUA.do(script_to_run, lua_state)

            send(main_process_id, {:script, ref, :return, parse_rtn_value(r, lstate)})
          rescue
            e ->
              Logging.debug("Exception happen it is :~p", [e])
              send(main_process_id, {:script, ref, :error, e})
          end
        end,
        priority: :low
      )

    receive do
      {:script, m, status, r} when m == ref ->
        Logging.debug("script returned with state:~p and result:~p", [status, r])
        {status, r}
    after
      script_run_timeout ->
        Logging.debug("script timeouted then kill it")
        Process.exit(pid, :kill)
        {:error, "timeout"}
    end
  end

  @compile {:inline, get_orig_message: 1}
  defp get_orig_message(_state) do
    Process.get(@orig_message_key)
  end

  @compile {:inline, set_orig_message: 2}
  defp set_orig_message(msg, _state) do
    Process.put(@orig_message_key, msg)
  end

  @compile {:inline, set_user_process_state: 1}
  defp set_user_process_state(state) do
    Process.put(:user_process_state, state)
  end

  @compile {:inline, get_user_process_state: 0}
  defp get_user_process_state() do
    Process.get(:user_process_state)
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

    result =
      list
      |> Enum.map(fn {_, x} -> x end)
      |> Enum.find(fn x -> x == item end) != nil

    {[result], state}
  end

  def logging(args, state) do
    Logging.debug("cel logging called with args:~p", [args])
    ups = get_user_process_state()
    Logging.debug("User Process State:~p", [ups])
    logging_queue = Kernel.get_in(ups.queues, [:cel_logging_Q])

    case logging_queue do
      nil ->
        Logging.debug("No Cel Logging Queue Found, ignore logging")

      _ ->
        [level, msg] = args
        Logging.debug("level:~p , msg:~p", [level, msg])

        data_to_enqueue = %{
          "level" => level,
          "message" => msg,
          "type" => "cel.logging",
          "time" => Utilities.now(),
          "module" => __MODULE__
        }

        Logging.debug("data_to_enqueue:~p", [data_to_enqueue])

        case DatabaseEngine.DurableQueue.enqueue(logging_queue, data_to_enqueue) do
          :nok ->
            Logging.warn("Problem to enqueue cel logging. data:~p", data_to_enqueue)

          _ ->
            Logging.debug("Successfully Enqueued.")
            :ok
        end
    end

    {[true], state}
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
    {[true], state}
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

  defp init_lua(msg, additional_functionality \\ %{}) do
    s0 = LUA.init()
    incoming_message = Utilities.Conversion.nested_map_to_tuple_list(msg)

    functionalities =
      Map.merge(
        additional_functionality,
        %{
          "incoming_message" => incoming_message,
          "is_subscribed" => &is_subscribed/2,
          "unsubscribe" => &unsubscribe/2,
          "in_array" => &in_array/2,
          "reply" => &reply/2,
          "log" => &logging/2,
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
        }
      )

    s1 =
      LUA.set_table(
        [:cel],
        Map.to_list(functionalities),
        s0
      )

    s1
  end
end
