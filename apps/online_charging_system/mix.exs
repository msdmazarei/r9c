defmodule OnlineChargingSystem.MixProject do
  use Mix.Project

  def project do
    [
      app: :online_charging_system,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:utilities, in_umbrella: true},
      {:database_engine, in_umbrella: true},
      {:dispatcher, in_umbrella: true},
      {
        :radius,
        github: "sigscale/radierl",
        compile: "aclocal; autoheader; autoconf; automake --add-missing; ./configure && make "
      }

      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true},
    ]
  end
end
