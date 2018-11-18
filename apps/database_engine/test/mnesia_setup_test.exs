defmodule MnesiaSetupTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias DatabaseEngine.Mnesia.DbSetup, as: DbSetup

  describe "Basic stop and stop" do
    setup do
      # set a temperary directory.
      Application.put_env(:mnesia, :dir, '/tmp/dbengine_unittests_#{UUID.uuid4()}')
      :ok

      on_exit(fn ->
        # test schema deletion.
        nodes = Utilities.allnodes()

        capture_log(fn ->
          assert :ok == DbSetup.delete_schema(nodes)
        end) =~ "database schema deleted."

        :ok
      end)
    end

    test "Mnesia setup process" do
      # test mnesia stop and also capture stop info log.
      capture_log(fn ->
        assert :stopped == DbSetup.stop_mnesia()
      end) =~ "Application mnesia exited: :stopped"

      # test mnesia start
      assert :ok == DbSetup.start_mnesia()

      assert capture_log(fn ->
               assert :ok == DbSetup.setup_everything()
             end) =~ "schema created. Loading ..."

      assert capture_log(fn ->
               :ok = DbSetup.setup_everything()
             end) =~ "database schema is already created."

      # lets populate database with some information

      nodes = Utilities.allnodes()
      # populate with 1000 records.
      DbSetup.populate_db(nodes)
      # assert the record length is 1000
      assert 1000 == TestTable |> :mnesia.dirty_all_keys() |> length
    end
  end
end
