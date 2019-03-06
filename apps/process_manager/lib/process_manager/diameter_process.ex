defmodule ProcessManager.Process.DiameterProcess do
  use ProcessManager.UnitProcess.GeneralUnitProcess

  def prepare_script_imutable_variables(
        inmsg = %DatabaseEngine.Models.DiameterPacket{},
        state
      ) do
    Logging.debug("called")
    %{"orig_message" => inmsg}
  end

  # def additional_functions(inmsg, state) do

  # end
end
