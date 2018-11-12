defmodule DatabaseEngine.Models.InternalCallback do
  defstruct module_name: "",
            function_name: "",
            arguments: []
end

defmodule DatabaseEngine.Models.SMS do
  @moduledoc false
  defstruct sender: nil,
            receiver: nil,
            sms_center: nil,
            sent_epoch: 0,
            received_epoch: 0,
            body: nil,
            id: nil,
            # options could contain below:
            #
            # for imi gateway:
            #    imi_charge_code: charge code
            #    imi_short_code: short code
            #    imi_service_key : service key
            #    imi_sms_type : :send_sms|:send_sms_without_charge
            options: %{},
            internal_callback: nil
end

defimpl Jason.Encoder,
  for: [
    DatabaseEngine.Models.SMS,
    DatabaseEngine.Models.OTP.VAS,
    DatabaseEngine.Models.InternalCallback
  ] do
  def encode(struct, opts) do
    m = Map.from_struct(struct)
    m = Map.put(m, :__orig_struct__, struct.__struct__)
    Jason.Encode.map(m, opts)
  end
end

defimpl DatabaseEngine.DurableQueue.Deserialize,
  for: [Map] do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def deserialize(data) do
    data =
      if is_map(data) do
        new_data =
          Enum.reduce(Map.to_list(data), %{}, fn {k, v}, acc ->
            new_v =
              if is_map(v) and v["__orig_struct__"] != nil do
                DatabaseEngine.DurableQueue.Deserialize.deserialize(v)
              else
                v
              end

            Map.put(acc, k, new_v)
          end)

        new_data
      else
        data
      end

    Logging.debug("data:~p", [data])

    struct_name = String.to_atom(data["__orig_struct__"])
    result = Utilities.to_struct(struct_name, data)

    r =
      case struct_name do
        DatabaseEngine.Models.SMS ->
          correct_options =
            for {key, val} <- result.options, into: %{}, do: {String.to_atom(key), val}

          correct_options =
            case correct_options[:imi_sms_type] do
              nil ->
                correct_options

              _ ->
                Map.put(
                  correct_options,
                  :imi_sms_type,
                  String.to_atom(correct_options[:imi_sms_type])
                )
            end

          Logging.debug("options:~p", [correct_options])

          %DatabaseEngine.Models.SMS{result | options: correct_options}

        _ ->
          result
      end
  end
end

defmodule DatabaseEngine.Models.OTP.VAS do
  defstruct contact: nil,
            service_name: nil,
            id: nil,
            options: %{},
            internal_callback: nil
end
