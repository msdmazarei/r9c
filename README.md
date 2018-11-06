# Red9

## TODO:
    - Create a true development environment including:
        - A dummy gateway to mimic network faults.
        - A dummy MO sending engine for test services.
        - A true stress test toolset.
        - Process monitoring toolset
    - Write Module names and functions to program the coding schedule between @rodmena and @msd

## Docs
- You can read architecture [document here](https://gitman.ir/sPod/red9-architecture/src/master/architecture.pdf).
- You can see architecture [digragram here](https://gitman.ir/sPod/red9-architecture/raw/master/diagram.png).
- Deployment wiki is [here](https://gitman.ir/sPod/red9-deployment-docs/wiki).

## VM requirements
- Download [FreeBSD-11.2-RELEASE-amd64 VHD image for VirtualBox](https://download.freebsd.org/ftp/releases/VM-IMAGES/11.2-RELEASE/amd64/Latest/FreeBSD-11.2-RELEASE-amd64.vhd.xz) and run it on VirtualBox or KVM.
- Please read architecture docs to understand the design goals and install a correct development environment.

- Install Elixir on every node.

- You can alternativly can clone red9-binaries and get [ready to use ova file](https://gitman.ir/sPod/red9-binaries) and import it in your VM.

## Starting platform

```bash
mix do clean, deps.clean --all, deps.get, deps.compile, compile, digest
```


We need at least 3 servers to start development:

```bash
iex --name [a|b|c]@127.0.0.1 --cookie 123 -S mix
```


Then you need to setup mnesia database. From node a:

```elixir
:ok = DatabaseEngine.Mnesia.DbSetup.delete_schema()
:ok = DatabaseEngine.Mnesia.DbSetup.setup_everything()
```

## Building binaries
 NOTE: TODO


## Description
This app has been created by `mix` tool and consists of multiple umbrella apps.

