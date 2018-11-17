defmodule MnesiaTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias DatabaseEngine.Mnesia.DbSetup, as: DbSetup

    setup_all do
      # set a temperary directory.
      Application.put_env(:mnesia, :dir, '/tmp/dbengine_unittests_#{UUID.uuid4()}')
      DbSetup.initialize
      DbSetup.create_tables
      assert :stopped = DbSetup.stop_mnesia
      assert :ok = DbSetup.start_mnesia
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

  describe "Auth" do

    test "creating and removing users" do
      :ok
      # test mnesia stop and also capture stop info log.
      # populate with 1000 records.
      #DbSetup.populate_db(nodes)
      # assert the record length is 1000
      #assert 1000 == TestTable |> :mnesia.dirty_all_keys() |> length
    end
  end
end
