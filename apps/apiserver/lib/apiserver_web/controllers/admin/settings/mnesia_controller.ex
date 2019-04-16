defmodule ApiserverWeb.Admin.Settings.Mnesia.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities.Admin.Mnesia, as: MnesiaAdmin

  @api_duration "api_duration"
  def badrpc_result(st_time, reason) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => %{
        "error" => :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()
      }
    }

    rtn
  end

  def ok(st_time, response) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => response
    }

    rtn
  end

  def get_mnesia_info_of_node(conn, params) do
    st_time = Utilities.now()

    {status, json_response} =
      case params["nodename"] do
        nodename when is_binary(nodename) ->
          nodename_atom = nodename |> String.to_atom()

          case MnesiaAdmin.get_mnesia_info(nodename_atom) do
            {:badrpc, reason} ->
              rtn = badrpc_result(st_time, reason)
              {500, rtn}

            v ->
              {200, ok(st_time, v)}
          end

        _ ->
          {400, %{"error" => %{"nodename" => "required"}}}
      end

    conn |> send_response(status, json_response)
  end

  def get_mnesia_table_info_on_node(conn, params) do
    st_time = Utilities.now()
    nodename = params["nodename"] |> String.to_atom()
    tbname = params["tbname"] |> String.to_atom()

    {status, json_response} =
      case Utilities.Admin.Mnesia.get_mnesia_table_info(nodename, tbname) do
        {:badrpc, reason} ->
          {500, badrpc_result(st_time, reason)}

        v ->
          {200, v}
      end

    conn |> send_response(status, json_response)
  end
end
