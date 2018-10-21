# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :apiserver,
  namespace: Apiserver

# Configures the endpoint
config :apiserver, Apiserver.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "VruaFd4lBYmtZgbPugQdnKdDJ9L56TcacwSc34ggmcgHdETnhssmsy6tcUcOQVO3",
  render_errors: [view: Apiserver.ErrorView, accepts: ~w(json)],
  pubsub: [name: Apiserver.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :apiserver, :generators,
  context_app: false

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
