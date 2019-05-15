defimpl ProcessManager.UnitProcess.Identifier,
  for: [
    DatabaseEngine.Models.SMS
  ] do

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def get_process_module(%DatabaseEngine.Models.SMS{}) do
    ProcessManager.Process.SMSProcess
  end

  def get_process_name(
        _data = %DatabaseEngine.Models.SMS{
          sender: sender
        }
      ) do
    sender
  end

  def get_identifier(
        _data = %DatabaseEngine.Models.SMS{
          id: id
        }
      ) do
    id
  end

  def get_script(_data, _state) do
    """
    print("hello world")
    return {a=1,b=2,c="hello"}
    """
  end

end
