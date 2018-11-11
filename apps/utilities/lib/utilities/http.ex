defmodule Utilities.HTTP1_1 do
  @moduledoc ""
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

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
        http_options \\ []
      ) do
    Logging.debug(
      "Called With Params: method:~p url:~p headers:~p content-type:~p http-options:~p",
      [method, url, headers, content_type, http_options]
    )

    Logging.debug("converting elixir string to erlang string")
    url = Utilities.to_erl_list(url)
    content_type = Utilities.to_erl_list(content_type)
    #    body = Utilities.to_erl_list(body)
    headers =
      headers |> Enum.map(fn {k, v} -> {Utilities.to_erl_list(k), Utilities.to_erl_list(v)} end)

    r =
      case body do
        n when n in ['', "", nil] ->
          Logging.debug("Call HTTP Target without body.")
          request = {url, headers}
          :httpc.request(method, request, http_options, httpc_options())

        _ ->
          Logging.debug("Call HTTP Target with body.")
          request = {url, headers, content_type, body}
          :httpc.request(method, request, http_options, httpc_options())
      end

    case r do
      {:ok, {{_http_version, status_code, reason_phrase}, resp_headers, resp_body}} ->
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

        Logging.debug("Return Result")
        {:ok, status_code, resp_headers, resp_body}

      {:error, reason} ->
        Logging.debug("There is a problem to HTTP Endpoint, Reason:~p", [reason])

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
end
