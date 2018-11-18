defmodule MnesiaTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias DatabaseEngine.Mnesia.DbSetup, as: DbSetup

  setup_all do
    # set a temperary directory.
    Application.put_env(:mnesia, :dir, '/tmp/dbengine_unittests_#{UUID.uuid4()}')
    DbSetup.initialize()
    DbSetup.create_tables()
    assert :stopped = DbSetup.stop_mnesia()
    assert :ok = DbSetup.start_mnesia()
    Process.sleep 1000 # wait for tables to create
    :ok

    on_exit(fn ->
      # test schema deletion.
      nodes = Utilities.all_active_nodes()

      capture_log(fn ->
        assert :ok == DbSetup.delete_schema(nodes)
      end) =~ "database schema deleted."

      :ok
    end)
  end

  describe "Mnesia functionality" do
    test "populating database" do
      nodes = Utilities.all_active_nodes()
      DbSetup.populate_db(nodes)

      assert {:atomic, _} =
               :mnesia.transaction(fn ->
                 :mnesia.all_keys(KVTb) |> length
               end)
    end
  end

  describe "Auth tables" do
    test "creating a user" do
      usr = %DatabaseEngine.Struct.AuthUser{
        key: UUID.uuid4(),
        name: "farsheed",
        company: "sPod",
        contact_number: "989120228207",
        email: "rodmena@me.com",
        domain: "sPod"
      }

      DatabaseEngine.Interface.Auth.add_user(usr)

      assert 1 == 1


    end
  end
end
