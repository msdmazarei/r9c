defmodule DatabaseEngine.Models do
  @moduledoc false

  defmodule SMS do
    @derive Jason.Encoder
    defstruct sender: "",
              receiver: "",
              sms_center: "",
              sent_epoch: 0,
              received_epoch: 0,
              body: ""
  end
end
