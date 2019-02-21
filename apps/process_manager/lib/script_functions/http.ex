defmodule ProcessManager.Script.Functionalities.HTTP do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  alias :httpc, as: HTTPC

  def lua_functionalities() do
    %{
      "http" => &http_request_check/2
    }
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
end
