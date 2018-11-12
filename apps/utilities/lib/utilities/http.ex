defmodule Utilities.HTTP1_1 do
  @moduledoc ""
  require Logger
  require Utilities.Logging
  require DatabaseEngine.DurableQueue
  alias Utilities.Logging

  @compile {:inline, authotization_header: 2}
  @compile {:inline, header: 2}
  @compile {:inline, authotization_header: 1}

  @http_config Application.get_env(:utilities, HTTP)
  @http_log_queue @http_config[:log_queue]

  @type method() :: :head | :get | :put | :post | :trace | :options | :delete | :patch
  @type key_value_list() :: [{String.t(), String.t()}]
  @type stri() :: String.t()
  @type url() :: stri()
  @type request() :: {url(), headers()} | {url(), headers(), content_type(), body()}
  @type status_line() :: {http_version(), status_code(), reason_phrase()}
  @type http_version() :: stri()
  @type status_code() :: integer()
  @type reason_phrase() :: stri()
  @type content_type() :: stri()
  @type headers() :: [header()]
  @type header() :: {field(), value()}
  @type field() :: stri()
  @type value() :: stri()
  @type accumulator() :: term()
  @type filename() :: stri()
  @type body_processing_result() :: :eof | {:ok, iolist(), accumulator()}
  @type body() ::
          stri()
          | {fun(accumulator()) :: body_processing_result(), accumulator()}
          | {:chunkify, fun(accumulator()) :: body_processing_result(), accumulator()}
  @type request_id() :: reference()
  @type passwordstring() :: String.t()
  @type userstring() :: String.t()
  @type http_options() :: [http_option()]
  @type http_option() ::
          {:timeout, timeout()}
          | {:connect_timeout, timeout()}
          | {:ssl, any()}
          | {:essl, any()}
          | {:autoredirect, boolean()}
          | {:proxy_auth, {userstring(), passwordstring()}}
          | {:version, http_version()}
          | {:relaxed, boolean()}
  @type result() :: {status_line(), headers(), body()} | {status_code(), body()} | request_id()
  @type reason() :: term()

  defp httpc_options() do
    []
  end

  @spec send_http_request(
          method(),
          url(),
          headers(),
          content_type(),
          body(),
          http_options()
        ) ::
          {:ok, status_code(), headers(), body()}
          | {:error, :failed_connect}
          | {:error, :timeout}
          | {:error, any()}

  def send_http_request(
        method,
        url,
        headers \\ [],
        content_type \\ nil,
        body \\ nil,
        http_options \\ [],
        id \\ nil
      ) do
    Logging.debug(
      "Called With Params: method:~p url:~p headers:~p content-type:~p http-options:~p",
      [method, url, headers, content_type, http_options]
    )

    http_options =
      if @http_config[:timeout] != nil do
        case List.keyfind(http_options, :timeout, 0) do
          v when v in [nil,{:timeout,nil}] ->
            [{:timeout, @http_config[:timeout]} | http_options]
          _ ->
            http_options
        end
      else
        http_options
      end

    Logging.debug("converting elixir string to erlang string")
    url = Utilities.to_erl_list(url)
    content_type = Utilities.to_erl_list(content_type)
    #    body = Utilities.to_erl_list(body)
    headers =
      headers |> Enum.map(fn {k, v} -> {Utilities.to_erl_list(k), Utilities.to_erl_list(v)} end)

    r =
      case body do
        n when n in ['', "", nil] ->
          request = {url, headers}
          Logging.debug("Call HTTP Target without body. request:~p", [request])

          if @http_log_queue != nil do
            # this will cause to crash if could not enqueue request and
            # force to restart worker
            :ok =
              DatabaseEngine.DurableQueue.enqueue(
                @http_log_queue,
                Utilities.nested_tuple_to_list({:request, id, request})
              )
              else
            Logging.warn("NO HTTP Q FOUND!! id:~p",[id])
          end

          :httpc.request(method, request, http_options, httpc_options())

        _ ->
          request = {url, headers, content_type, body}
          Logging.debug("Call HTTP Target with body. request:~p", [request])

          if @http_log_queue != nil do
            # this will cause to crash if could not enqueue request and
            # force to restart worker
            :ok =
              DatabaseEngine.DurableQueue.enqueue(
                @http_log_queue,
                Utilities.nested_tuple_to_list({:request, id, request})
              ) else
            Logging.warn("NO HTTP Q FOUND!! id:~p",[id])

          end

          :httpc.request(method, request, http_options, httpc_options())
      end

    case r do
      {:ok, {{_http_version, status_code, _reason_phrase}, resp_headers, resp_body}} ->
        Logging.debug(
          "Http Endpoint respond and we are converting erlang string to elixir string"
        )

        resp_headers =
          resp_headers
          |> Enum.map(fn {k, v} ->
            {Utilities.erl_list_to_iex_string(k), Utilities.erl_list_to_iex_string(v)}
          end)

        resp_body = Utilities.erl_list_to_iex_string(resp_body)

        resp_body =
          case List.keyfind(resp_headers, "content-encoding", 0) do
            nil ->
              Logging.debug("there is no compression in body.")
              resp_body

            {_, "gzip"} ->
              Logging.debug("Body is compressed by gzip. unzipping it ")
              :zlib.gunzip(resp_body)

            body_encoding ->
              Logging.debug("body encoding is : ~p . return it directly", [body_encoding])
              resp_body
          end

        result = {:ok, status_code, resp_headers, resp_body}

        Logging.debug("Return Res:~p", [result])

        if @http_log_queue != nil do
          case DatabaseEngine.DurableQueue.enqueue(
                 @http_log_queue,
                 Utilities.nested_tuple_to_list({:response, id, result})
               ) do
            :ok ->
              :ok

            v ->
              Logging.warn("could not enqueue http_response for id: ~p , queue response:~p", [
                id,
                v
              ])
          end
        end

        result

      {:error, reason} ->
        Logging.debug("There is a problem to HTTP Endpoint, Reason:~p", [reason])
        Logging.warn("problem in http connection: ~p", [reason])

        if @http_log_queue != nil do
          case DatabaseEngine.DurableQueue.enqueue(
                 @http_log_queue,
                 Utilities.nested_tuple_to_list({:response, id, {:error, reason}})
               ) do
            :ok ->
              :ok

            v ->
              Logging.warn("could not enqueue http_response for id: ~p , queue response:~p", [
                id,
                v
              ])
          end
          else
          Logging.warn("NO HTTP Q FOUND!! id:~p",[id])

        end

        case reason do
          {:failed_connect, l} when is_list(l) ->
            case List.keyfind(l, :inet, 0) do
              {:inet, _, :timeout} ->
                {:error, :timeout}

              _ ->
                {:error, :failed_connect}
            end

          _ ->
            {:error, reason}
        end
    end
  end

  @spec post(url(), headers(), content_type(), body(), integer()) ::
          {:ok, status_code(), headers(), body()}
          | {:error, :failed_connect}
          | {:error, :timeout}
          | {:error, any()}
  def post(url, headers \\ [], content_type, body, timeout \\ nil, id \\ nil) do
    Logging.debug("Called With params: url:~p content_type:~p timeout:~p", [
      url,
      content_type,
      timeout
    ])

    send_http_request(:post, url, headers, content_type, body, [{:timeout, timeout}], id)
  end

  @spec wsdl(url(), String.t(), headers(), integer) ::
          {:ok, status_code(), headers(), body()}
          | {:error, :failed_connect}
          | {:error, :timeout}
          | {:error, any()}
  def wsdl(url, action, headers \\ [], body, timeout \\ nil, id \\ nil) do
    Logging.debug("Called url:~p action:~p headers:~p timeout:~p", [url, action, headers, timeout])

    headers = [{"SOAPAction", action} | headers]
    post(url, headers, "text/xml;charset=UTF-8", body, timeout, id)
  end

  @spec get(url(), headers(), integer) ::
          {:ok, status_code(), headers(), body()}
          | {:error, :failed_connect}
          | {:error, :timeout}
          | {:error, any()}
  def get(url, headers, timeout \\ 100_000) do
    Logging.debug("Called url:~p headers:~p timeout:~p", [url, headers, timeout])
    send_http_request(:get, url, headers, nil, nil, [{:timeout, timeout}])
  end

  @spec header(field(), value()) :: header()
  def header(field, value) do
    {field, value}
  end

  @spec authotization_header(:basic | :bearer, value()) :: header()
  def authotization_header(type, value) do
    header("Authorization", to_string(type) <> " " <> value)
  end

  @spec authotization_header(value()) :: header()
  def authotization_header(value) do
    header("Authorization", value)
  end
end
