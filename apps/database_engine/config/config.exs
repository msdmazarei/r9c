# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :database_engine, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:database_engine, :key)
#
# You can also configure a 3rd-party app:
#
config :logger, level: :debug
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env()}.exs"
#
config :mnesia,
  dir: 'STORAGE/RED9_COBRA_db_#{Mix.env()}_#{node()}',
  dc_dump_limit: 512

config :kafka_ex,
  # A list of brokers to connect to. This can be in either of the following formats
  #
  #  * [{"HOST", port}...]
  #  * CSV - `"HOST:PORT,HOST:PORT[,...]"`
  #  * {mod, fun, args}
  #  * &arity_zero_fun/0
  #  * fn -> ... end
  #
  # If you receive :leader_not_available
  # errors when producing messages, it may be necessary to modify "advertised.host.name" in the
  # server.properties file.
  # In the case below you would set "advertised.host.name=localhost"
  #       {"msd.pc.rashavas.office", 9092}

  brokers: [
    {"s1.kafka.local", 9092}
  ],
  #
  # OR:
  # brokers: "localhost:9092,localhost:9093,localhost:9094"
  #
  # It may be useful to configure your brokers at runtime, for example if you use
  # service discovery instead of storing your broker hostnames in a config file.
  # To do this, you can use `{mod, fun, args}` or a zero-arity function, and `KafkaEx`
  # will invoke your callback when fetching the `:brokers` configuration.
  # Note that when using this approach you must return a list of host/port pairs.
  #
  # the default consumer group for worker processes, must be a binary (string)
  #    NOTE if you are on Kafka < 0.8.2 or if you want to disable the use of
  #    consumer groups, set this to :no_consumer_group (this is the
  #    only exception to the requirement that this value be a binary)
  consumer_group: "kafka_ex",
  # Set this value to true if you do not want the default
  # `KafkaEx.Server` worker to start during application start-up -
  # i.e., if you want to start your own set of named workers
  disable_default_worker: false,
  # Timeout value, in msec, for synchronous operations (e.g., network calls).
  # If this value is greater than GenServer's default timeout of 5000, it will also
  # be used as the timeout for work dispatched via KafkaEx.Server.call (e.g., KafkaEx.metadata).
  # In those cases, it should be considered a 'total timeout', encompassing both network calls and
  # wait time for the genservers.
  sync_timeout: 3000,
  # Supervision max_restarts - the maximum amount of restarts allowed in a time frame
  max_restarts: 10,
  # Supervision max_seconds -  the time frame in which :max_restarts applies
  max_seconds: 60,
  # Interval in milliseconds that GenConsumer waits to commit offsets.
  commit_interval: 5_000,
  # Threshold number of messages consumed for GenConsumer to commit offsets
  # to the broker.
  commit_threshold: 100,
  # This is the flag that enables use of ssl
  use_ssl: false,
  # see SSL OPTION DESCRIPTIONS - CLIENT SIDE at http://erlang.org/doc/man/ssl.html
  # for supported options
  ssl_options: [],
  # set this to the version of the kafka broker that you are using
  # include only major.minor.patch versions.  must be at least 0.8.0
  kafka_version: "0.10.1"

config :couchdb_connector,
  props: %{
    protocol: "http",
    hostname: "localhost",
    database: "red9_cobra_#{Mix.env()}",
    port: 5984
  }
