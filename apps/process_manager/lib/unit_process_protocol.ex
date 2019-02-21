defprotocol ProcessManager.UnitProcess.Identifier do
  def get_process_name(data)
  def get_identifier(data)

  def get_script(data, state)
end

defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.RadiusPacket
  ] do
  def get_identifier(data = %DatabaseEngine.Models.RadiusPacket{}) do
    :io_lib.format("~p-~p", [data.authenticator, data.id])
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
    print("simple script")
    """
  end
end
