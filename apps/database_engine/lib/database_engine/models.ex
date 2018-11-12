defmodule DatabaseEngine.Models do
  @moduledoc false


  defmodule SMS do
    @derive Jason.Encoder
    defstruct sender: "" ,
              receiver: "",
              sms_center: "",
              sent_epoch: 0,
              received_epoch: 0,
              body: ""

  end



defmodule DatabaseEngine.Models.VAS.OTP do
  defstruct contact: nil,
            service_name: nil,
            id: nil,
            options: %{}
end
