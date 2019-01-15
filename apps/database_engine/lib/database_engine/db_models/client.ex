defmodule DatabaseEngine.DbModels.Client do
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Mnesia.Records
  alias DatabaseEngine.Mnesia.Records

  require Record

  defstruct id: nil,
            name: nil,
            is_active: nil,
            is_company: nil,
            create_unixepoch: nil,
            modify_unixepoch: nil,
            user_customdata: nil,
            additional_props: nil,
            version: nil

  def from_mnesia_record(data) do
    Logging.debug("Called. data:~p",[data])

    case Records.client_tb(data, :_internal) do
      nil ->
        Logging.debug("_internal field is null and there is nothing there")
        nil

      m when is_map(m) ->
      Logging.debug("_internal field of record is map.")
        m |> Map.get("full_record")

      _ ->
        Logging.debug("_internal field is not map.")
        nil
    end
  end

  defimpl Jason.Encoder do
    def encode(struct, opts) do
      m = Map.from_struct(struct)
      m = Map.put(m, :__orig_struct__, struct.__struct__)
      Jason.Encode.map(m, opts)
    end
  end

  defimpl DatabaseEngine.MnesiaWrapper.Serializer do
    def to_mnesia_record(
          data = %DatabaseEngine.DbModels.Client{
            id: id,
            name: name,
            create_unixepoch: create_unixepoch,
            version: version
          }
        ) do
      Logging.debug("Called")

      Records.client_tb(
        idx: id,
        create_unixepochx: create_unixepoch,
        version: version,
        name: name,
        _internal: %{
          "full_record" => data
        }
      )
    end
  end
end
