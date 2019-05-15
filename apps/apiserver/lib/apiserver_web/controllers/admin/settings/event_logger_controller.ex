defmodule ApiserverWeb.Admin.Settings.EventLogger.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require Utilities

  require DatabaseEngine

  require DatabaseEngine.Utils.EventLogger
  alias DatabaseEngine.Utils.EventLogger

  @api_duration "api_duration"
  def params_to_struct(params) do
    Utilities.to_struct(Model, params)
  end

  def response_body(st_time, result) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => result
    }

    rtn
  end

  def new_log_event(conn, params) do
    Logging.debug("called.")
    st_time = Utilities.now()

    EventLogger.log_event(
      params["entity_name"],
      params["entity_id"],
      params["state"],
      params["data"]
    )

    result = response_body(st_time, %{})

    conn |> send_response(200, result)
  end
end
