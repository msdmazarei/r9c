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
      version: 1,
      create_unixepoch: Utilities.now(),
      id: UUID.uuid4()
    }

    record = Serializer.to_mnesia_record(instance)

    true = MnesiaWrapper.write(record)

    instance
  end

  def get_by_id(id) do
    Logging.debug("Called, id:~p", [id])

    case MnesiaWrapper.read(get_tb_name(), id) do
      [] ->
        Logging.debug("nothing found, returns nil.")
        nil

      [record] when Record.is_record(record) ->
        Logging.debug("one record found. return 1 record.")
        ClientModel.from_mnesia_record(record)
    end
  end

  @spec delete(any(), number) :: boolean() | :bad_version | nil
  def delete(id, version) do
    Logging.debug("Called. to delete id: ~p", [id])

    case get_by_id(id) do
      nil ->
        nil

      instance ->
        if instance.version == version do
          MnesiaWrapper.delete(get_tb_name(), id)
        else
          Logging.debug("instance.version: ~p requested_version:~p",[instance.version,version])
          :bad_version
        end
    end
  end


  def update(instance,map_for_updating) do
    Logging.debug("Called.")
    edited_instance = Utilities.update_struct(instance,map_for_updating)
    Logging.debug("increase version")
    edited_instance = %{edited_instance | version: (edited_instance.version  || 1) + 1 }

    record = Serializer.to_mnesia_record(edited_instance)

    true = MnesiaWrapper.write(record)
    Logging.debug("stored in mnesia successfully")
    edited_instance

  end
end
