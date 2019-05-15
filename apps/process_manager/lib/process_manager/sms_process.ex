defmodule ProcessManager.Process.SMSProcess do
  use ProcessManager.UnitProcess.GeneralUnitProcess

  def prepare_script_imutable_variables(
        inmsg = %DatabaseEngine.Models.SMS{
        },
        state
      ) do
    Logging.debug("called")
    %{"orig_message" => inmsg}
  end

  # def additional_functions(inmsg, state) do

  # end

  def test_multiple_event() do
    e1 = event_log("name","id","state","s0","s1")
    EventLogger.log_events([e1,e1])
  end
end
