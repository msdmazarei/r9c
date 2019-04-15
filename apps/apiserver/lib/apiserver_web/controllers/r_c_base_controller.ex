defmodule ApiserverWeb.RCBaseController do
  @moduledoc false

  defmacro __using__(_ \\ []) do
    quote do
      require Logger
      require Utilities.Logging
      alias Utilities.Logging
      require Plug.Conn
      alias Plug.Conn


      import Phoenix.Controller




      use ApiserverWeb, :controller
      #      plug Plug.Parsers, parsers: [:urlencoded, :json],
      #                         pass: ["text/*"],
      #                         json_decoder: Jason
      #
      def request_body_validation(conn, params) do
        Logging.debug(
          "Called, host:~p method:~p path_info:~p scrip_name:~p request_path:~p, params:~p, body_params:~p",
          [
            conn.host,
            conn.method,
            conn.path_info,
            conn.script_name,
            conn.request_path,
            params,
            conn.body_params
          ]
        )

        req_body = conn.body_params

        schema_file =
          Path.join([
            Path.dirname(__ENV__.file),
            "json_schemas",
            conn.method <> "_" <> (conn.path_info |> Enum.join("_")) <> ".json"
          ])

        Logging.debug("json validation schema path: ~p", [schema_file])

        if File.exists?(schema_file) do
          case File.read(schema_file) do
            {:ok, file_content} ->
              case file_content |> Jason.decode() do
                {:ok, parsed_json} ->
                  case parsed_json |> ExJsonSchema.Validator.valid?(req_body) do
                    true ->
                      Logging.debug("valid request")
                      conn

                    false ->
                      Logging.debug("invalid request, return 400")
                      {_, errors} = ExJsonSchema.Validator.validate(parsed_json, req_body)
                      Logging.debug("errors:~p", [errors])

                      rtn =
                        errors
                        |> Enum.reduce(%{}, fn {e, fieldpath}, acc ->
                          if acc |> Map.has_key?(fieldpath) do
                            case acc |> Map.get(fieldpath) do
                              v when is_list(v) ->
                                acc |> Map.put(fieldpath, [e | v])

                              v ->
                                acc |> Map.put(fieldpath, [v, e])
                            end
                          else
                            acc |> Map.put(fieldpath, e)
                          end
                        end)

                      conn
                      |> put_resp_content_type("application/json")
                      |> put_status(400)
                      |> json(rtn)
                      |> halt()
                  end

                _ ->
                  Logging.debug("Halt Conn. Problem to parsing json")
                  conn |> Conn.put_status(500) |> Conn.halt()
              end

            _ ->
              Logging.debug("Halt Conn. problem in reading schema file: ~p.", [schema_file])
              conn |> Conn.put_status(500) |> Conn.halt()
          end
        else
          Logging.debug("schema file does not exists, consider body as corrected one.")
          conn
        end
      end

      def send_response(conn, status, json_response) do
        case json_response do
          nil ->
            send_resp(conn, status, "")

          _ ->
            conn |> put_status(status) |> json(json_response)
        end
      end
    end
  end

  #  defmacro json_validate(body) do
  #    quote do
  #      if File.exists?(
  #           Path.join([
  #             Path.dirname(__ENV__.file),
  #             "json_schemas",
  #             Atom.to_string(elem(__ENV__.function, 0))
  #           ]) <> ".json"
  #         ) do
  #        schemapath =
  #          Path.join([
  #            Path.dirname(__ENV__.file),
  #            "json_schemas",
  #            Atom.to_string(elem(__ENV__.function, 0))
  #          ]) <> ".json"
  #
  #        case File.read(schemapath) do
  #          {:ok, file_content} ->
  #            case file_content |> Jason.encode() do
  #              {:ok, parsed_json} ->
  #                parsed_json |> ExJsonSchema.Validator.valid?(unquote(body))
  #
  #              _ ->
  #                Logging.debug("Problem to parsing json")
  #                false
  #            end
  #
  #          _ ->
  #            Logging.debug("problem in reading schema file: ~p", [schemapath])
  #            false
  #        end
  #      else
  #        Logging.debug("no schema file found")
  #        true
  #      end
  #    end
  #  end
end
