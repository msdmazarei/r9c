defmodule ApiserverWeb.SMS.MO.IMI.Controller do
  use ApiserverWeb.RCBaseController, :crud

  alias ApiserverWeb.RCBaseController

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def imi_mo(conn, _params) do
    {:ok, body, conn} = conn |> read_body
    doc = body |> Exml.parse()
    addressT = Exml.get(doc, "//loc:addresses")
    sender_nameT = Exml.get(doc, "//loc:senderName")
    raw_messageT = Exml.get(doc, "//loc:message")
    s2 = [addressT, sender_nameT, raw_messageT]
    res = s2 |> Enum.filter(fn x -> is_list(x) == true end) |> Enum.zip()

    set =
      case res do
        [] ->
          [{addressT, sender_nameT, raw_messageT}]

        data ->
          data
      end

    for each <- set do
      {address, sender_name, raw_message} = each

      message =
        raw_message
        |> Utilities.Conversion.convert_hex_to_unicode()
        |> Utilities.Conversion.Persian.fix()

        #enqueue all messages to input queue
      # |> String.slice(0,32)  ## mo must be short
      case Regex.named_captures(~r/(?<ac>\d{2})(?<nn>\d{10})/, address) do
        %{"ac" => area_code, "nn" => national_number} ->
          Red9.MessageTools.do_react(sender_name, message, national_number, area_code)

        nil ->
          :continue
      end
    end

    ## ----------------------------------------------------------------
    conn
    |> send_resp(200, "OK")
  end

end
