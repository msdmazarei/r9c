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
  @wait_time 500
  # @data File.read!("/Users/rodmena/Movies/BW-Scroll.mp4") |> Base.encode64
  #

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
        :rpc.call(n, __MODULE__, :start_mnesia, [])
      end)

    :ok
  end

  @spec stop_every_mnesia(list(Atom.t())) :: :ok
  def stop_every_mnesia(nodes) do
    _ =
      nodes
      |> Enum.map(fn n ->
        :rpc.call(n, __MODULE__, :stop_mnesia, [])
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

  @spec insert_test_record() :: {:atomic, :ok}
  def insert_test_record do
    # idx = UUID.uuid4()
    key = :hasan
    value = :rahim

    pck = {KVTb, key, value}

    {:atomic, :ok} =
      :mnesia.transaction(fn ->
        # Logger.debug(fn -> "test record #{idx} is inserting to database" end)
        :ok = :mnesia.write(pck)
      end)
  end

  ############################# SDP TABLES #############################
  @table_config [
    {AuthUserTb, [:idx, :key_idx, :domain_idx, :options, :_internal]},
    {AuthGroupTb, [:idx, :name_idx]},
    {AuthMembershipTb, [:idx, :user_idx, :group_idx]},
    {AuthPermissionTb, [:idx, :group_idx, :role_idx]},
    {ClientTb, [:idx, :create_unixepochx, :version, :name, :_internal]},
    {MembershipTb, [:idx, :source_idx, :target_idx, :scope_idx]},
    {NetAclTb, [:idx, :cidr_idx, :action_idx, :_internal]},
    {CelTb, [:idx, :cel]},
    {GatewayTb, [:idx, :type_idx, :cel_id, :options, :_internal]},
    {ServiceTb, [:idx, :type_idx, :cel_id, :whitelist, :options, :_internal]},
    {AppTb, [:idx, :service_idx, :apikeys, :options, :_internal]},
    {ApikeyTb, [:idx, :key_idx, :net_acl_idx, :options, :_internal]},
    {ProcessTb, [:idx, :model]},
    {SubscriptionTb,
     [
       :idx,
       :service_idx,
       :client_idx,
       :start_unixtime,
       :end_unixtime,
       :status_idx,
       :correlator,
       :flag_idx
     ]},
    {EventTb, [:idx, :correlator_idx]},
    {KVTb, [:key, :value]}
  ]

  def create_tables do
    for tdata <- @table_config do
      name = tdata |> elem(0)
      attrs = tdata |> elem(1)
      idxs = attrs |> Enum.filter(fn x -> x |> to_string |> String.ends_with?("_idx") end)

      case :mnesia.create_table(name, [
             {:disc_copies, Utilities.all_active_nodes()},
             {:type, :ordered_set},
             majority: true,
             attributes: attrs,
             index: idxs
           ]) do
        {:atomic, :ok} ->
          Logger.info(fn -> "#{name} table created." end)

        {:aborted, {:already_exists, name}} ->
          Logger.debug(fn -> "#{name} table is available." end)
      end
    end
  end

  @spec initialize() :: :ok
  def initialize do
    nodes = Utilities.all_active_nodes()
    create_schema(nodes)
    Logger.info(fn -> "schema created. Loading ..." end)
    Process.sleep(@wait_time)
    # create_tables()
    :ok
  end

  @spec populate_db(list(Atom.t())) :: any()
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
