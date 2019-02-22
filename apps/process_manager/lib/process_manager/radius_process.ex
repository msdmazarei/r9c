defmodule ProcessManager.Process.RadiusProcess do
  use ProcessManager.UnitProcess.GeneralUnitProcess

  def jafar() do
    Utilities.callback(
      %{},
      "Elixir.OnlineChargingSystem.RadiusServer",
      "reply_result_after_process",
      [1]
    )
  end
end
