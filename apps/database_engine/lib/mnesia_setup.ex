defmodule DatabaseEngine.Mnesia.DbSetup do
  require Logger

  [{:database, [_,{:config, [hosts: nodes]},_,_,_]}] = Application.get_env(:libcluster, :topologies) 
  @nodes nodes
  # @data File.read!("/Users/rodmena/Movies/BW-Scroll.mp4") |> Base.encode64
  #

  def setup_everything do
    create_schema()
    Logger.info(fn -> "schema created. Loading ..." end)
    Process.sleep(2_000)
    create_test_table()
  end

  def connect_to_all do
    [_ | friends] = @nodes
    friends |> Enum.map(fn n -> Node.ping(n) end)
  end

  def stop_mnesia do
    :stopped = :mnesia.stop()
  end

  def start_mnesia do
    :ok = :mnesia.start()
  end

  def start_every_mnesia do
    @nodes
    |> Enum.map(fn n ->
      Node.spawn(n, __MODULE__, :start_mnesia, [])
    end)
  end

  def stop_every_mnesia do
    connect_to_all()

    @nodes
    |> Enum.map(fn n ->
      Node.spawn(n, __MODULE__, :stop_mnesia, [])
    end)
  end

  def delete_schema do
    connect_to_all()
    stop_every_mnesia()

    case :mnesia.delete_schema(@nodes) do
      :ok ->
        Logger.warn("database schema deleted.")
    end

    start_every_mnesia()
  end

  def create_schema do
    connect_to_all()
    stop_every_mnesia()
    Logger.info(fn -> "stopping nodes ..." end)
    Process.sleep(2_000)

    case :mnesia.create_schema(@nodes) do
      :ok ->
        Logger.info(fn -> "database schema created." end)

      {:error, {_, {:already_exists, _}}} ->
        Logger.debug(fn -> "database schema is already created." end)
    end

    start_every_mnesia()
  end

  def create_test_table do
    connect_to_all()

    case :mnesia.create_table(TestTable, [
           {:disc_copies, @nodes},
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

  def populate_db do
    for _ <- 1..100 do
      Node.spawn(@nodes |> Enum.shuffle() |> hd, fn ->
        for _ <- 1..10 do
          insert_test_record()
        end
      end)
    end
  end
end
