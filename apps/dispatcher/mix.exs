defmodule Dispatcher.MixProject do
  use Mix.Project

  def project do
    [
      app: :dispatcher,
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
  def included_applications do
    [
      extra_applications: [:logger],
      mod: {Dispatcher.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:luerl,
       git: "https://github.com/rvirding/luerl",
       compile: "make && cp src/luerl.app.src ebin/luerl.app"},
      {:utilities, in_umbrella: true},
      {:database_engine, in_umbrella: true},
      {:networking, in_umbrella: true},
      {:gateway_core, in_umbrella: true},
      {:process_manager, in_umbrella: true}

      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true},
    ]
  end
end
