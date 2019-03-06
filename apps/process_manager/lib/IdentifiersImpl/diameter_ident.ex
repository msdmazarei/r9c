defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.DiameterPacket
  ] do
  def get_process_name(
        _data = %DatabaseEngine.Models.DiameterPacket{
          id: id
        }
      ) do
    id
  end

  def get_identifier(
        _data = %DatabaseEngine.Models.DiameterPacket{
          id: id
        }
      ) do
    id
  end

  def get_script(_data, _state) do
    ""
  end
end
