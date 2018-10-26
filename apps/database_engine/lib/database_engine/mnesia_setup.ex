defmodule DatabaseEngine.Mnesia.DbSetup do
  @moduledoc """
  Creates and setups new mnesia database for project.
  What you need is to run :setup_everything function.
  You will also have a Mix task for it.
  """
  require Logger

  # [{:database, [_, {:config, [hosts: nodes]}, _, _, _]}] =
  #  Application.get_env(:libcluster, :topologies)

  # @nodes nodes
  @wait_time 1_000
  # @data File.read!("/Users/rodmena/Movies/BW-Scroll.mp4") |> Base.encode64
  #

  @spec setup_everything() :: :ok
  def setup_everything do
    nodes = Utilities.allnodes()
    create_schema(nodes)
    Logger.info(fn -> "schema created. Loading ..." end)
    Process.sleep(@wait_time)
    create_test_table(nodes)
    :ok
  end

  @spec stop_mnesia() :: :stopping
  def stop_mnesia do
    :stopped = :mnesia.stop()
  end

  @spec start_mnesia() :: :ok
  def start_mnesia do
    :ok = :mnesia.start()
  end

  @spec start_every_mnesia(list(Atom.t())) :: :ok
  def start_every_mnesia(nodes) do
    _ =
      nodes
      |> Enum.map(fn n ->
        Node.spawn(n, __MODULE__, :start_mnesia, [])
      end)

    :ok
  end

  @spec stop_every_mnesia(list(Atom.t())) :: :ok
  def stop_every_mnesia(nodes) do
    _ =
      nodes
      |> Enum.map(fn n ->
        Node.spawn(n, __MODULE__, :stop_mnesia, [])
      end)

    :ok
  end

  @spec delete_schema(list(Atom.t())) :: :ok
  def delete_schema(nodes) do
    stop_every_mnesia(nodes)

    Logger.info(fn -> "stopping nodes ..." end)
    Process.sleep(@wait_time)

    case :mnesia.delete_schema(nodes) do
      :ok ->
        Logger.warn("database schema deleted.")
    end

    start_every_mnesia(nodes)
  end

  @spec create_schema(list(Atom.t())) :: :ok
  def create_schema(nodes) do
    stop_every_mnesia(nodes)
    Logger.info(fn -> "stopping nodes ..." end)
    Process.sleep(@wait_time)

    case :mnesia.create_schema(nodes) do
      :ok ->
        Logger.info(fn -> "database schema created." end)

      {:error, {_, {:already_exists, _}}} ->
        Logger.debug(fn -> "database schema is already created." end)
    end

    start_every_mnesia(nodes)
  end

  @spec create_test_table(list(Atom.t())) :: :ok
  def create_test_table(nodes) do
    case :mnesia.create_table(TestTable, [
           {:disc_copies, nodes},
           majority: true,
           attributes: [:data1, :data2, :data3, :data4],
           index: [:data3]
         ]) do
      {:atomic, :ok} ->
        Logger.info(fn -> "test table created" end)

      {:aborted, {:already_exists, TestTable}} ->
        Logger.debug(fn -> "test table is available." end)
    end

    :ok
  end

  @spec insert_test_record() :: {:atomic, :ok}
  def insert_test_record do
    idx = UUID.uuid4()
    data2 = :hasan
    data4 = :rahim
    data3 = :gholi

    pck = {TestTable, idx, data2, data3, data4}

    {:atomic, :ok} =
      :mnesia.transaction(fn ->
        :ok = :mnesia.write(pck)
      end)
  end

  @spec populate_db(list(Atom.t())) :: term
  def populate_db(nodes) do
    for _ <- 1..1000 do
      Utilities.randseed()

      :rpc.call(
        nodes |> Enum.shuffle() |> hd,
        DatabaseEngine.Mnesia.DbSetup,
        :insert_test_record,
        []
      )
    end
  end
end
