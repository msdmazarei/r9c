defmodule DatabaseEngine.Repositories.Mnesia.Client do
  require DatabaseEngine.DbModels.Client
  alias DatabaseEngine.DbModels.Client, as: ClientModel

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.MnesiaWrapper
  alias DatabaseEngine.MnesiaWrapper

  require DatabaseEngine.MnesiaWrapper.Serializer
  alias DatabaseEngine.MnesiaWrapper.Serializer

  require Record

  def get_tb_name() do
    record = Serializer.to_mnesia_record(%ClientModel{})
    elem(record, 0)
  end

  @spec add_new(any(), boolean(), boolean()) :: DatabaseEngine.DbModels.Client.t()
  def add_new(name, is_company, is_active)
      when is_boolean(is_company) and is_boolean(is_active) do
    Logging.debug("called. with params. name: ~p is_company:~p is_active:~p", [
      name,
      is_company,
      is_active
    ])

    instance = %ClientModel{
      name: name,
      is_company: is_company,
      is_active: is_active,
      create_unixepoch: Utilities.now(),
      id: UUID.uuid4()
    }

    record = Serializer.to_mnesia_record(instance)

    :ok = MnesiaWrapper.write(record)

    instance
  end

  def get_by_id(id) do
    case MnesiaWrapper.read(get_tb_name(), id) do
      [] ->
        nil

      [record] when Record.is_record(record) ->
        ClientModel.from_mnesia_record(record)
    end
  end
end
