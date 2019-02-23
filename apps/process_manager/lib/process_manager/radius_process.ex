defmodule ProcessManager.Process.RadiusProcess do
  use ProcessManager.UnitProcess.GeneralUnitProcess

  def prepare_script_imutable_variables(
        inmsg = %DatabaseEngine.Models.RadiusPacket{
          attribs: attribs
        },
        state
      ) do
    Logging.debug("called")
    %{"orig_message" => inmsg, "orig_message_attribs" => attribs}
  end

  # def additional_functions(inmsg, state) do

  # end
end
