defimpl Dispatcher.Protocols.DispatcherInfo,
  for: [
    DatabaseEngine.Models.DiameterPacket
  ] do
    require Logger
    require Utilities.Logging
    alias Utilities.Logging

    def get_unit_process_module_for_message(%DatabaseEngine.Models.DiameterPacket{}) do
      ProcessManager.Process.DiameterProcess

    end
    def get_process_name(%DatabaseEngine.Models.DiameterPacket{}) do

    end
  end
