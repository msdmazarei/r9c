defprotocol ProcessManager.UnitProcess.Identifier do

  def get_process_module(data)
  @spec get_process_name(any()) :: any()
  def get_process_name(data)
  def get_identifier(data)

  def get_script(data, state)
end
