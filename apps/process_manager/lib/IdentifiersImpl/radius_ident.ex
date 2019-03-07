defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.RadiusPacket
  ] do
  def get_process_module(%DatabaseEngine.Models.RadiusPacket{}) do
    ProcessManager.Process.RadiusProcess
  end

  def get_identifier(data = %DatabaseEngine.Models.RadiusPacket{}) do
    md5 =
      :crypto.hash(
        :md5,
        data |> Utilities.Serializers.BinSerializer.serialize()
      )
      |> Base.encode16()

    Utilities.erl_list_to_iex_string(:io_lib.format("id:~p-s:~s", [data.id, md5]))
  end

  def get_process_name(data = %DatabaseEngine.Models.RadiusPacket{}) do
    username_attr_id = 1
    acct_session_id = 44
    acct_multi_session_id = 50

    data.attribs[username_attr_id] || data.attribs[acct_session_id] ||
      data.attribs[acct_multi_session_id]
  end

  def get_script(data = %DatabaseEngine.Models.RadiusPacket{}, _) do
    """

    function dump(o)
      if type(o) == 'table' then
          local s = '{ '
          for k,v in pairs(o) do
            if type(k) ~= 'number' then k = '"'..k..'"' end
            s = s .. '['..k..'] = ' .. dump(v) .. ','
          end
          return s .. '} '
      else
          return tostring(o)
      end
    end


    function mikrotik_response()

    end



    function authentication()
    print("authenticate called.")
    cel = _G.cel
    print("incoming message: ", dump(cel.incoming_message))

    r_username_attr = 1

    r_framed_protocol_attr = 7

    framed_protocol_ppp_val = 1

    r_framed_comporession = 13

    framed_compression_van_jacobsen_tcp_ip_val = 1


    auth_response = {}
    auth_response[r_framed_protocol_attr] = cel.radius.int_attr( framed_protocol_ppp_val , 4 )
    auth_response[r_framed_comporession ] = cel.radius.int_attr( framed_compression_van_jacobsen_tcp_ip_val, 4)

    username = cel.radius.get_str_attr( r_username_attr )


    return cel.radius.response(2, auth_response)
    end




    if (cel.incoming_message.code == 1 ) then
      return authentication()
    end


    """
  end
end
