# Red9

## Docs
- You can read architecture [document here](https://gitman.ir/sPod/red9-architecture/src/master/architecture.pdf).
- You can see architecture [digragram here](https://gitman.ir/sPod/red9-architecture/raw/master/diagram.png).
- Deployment wiki is [here](https://gitman.ir/sPod/red9-deployment-docs/wiki).

## VM requirements
- Download [FreeBSD-11.2-RELEASE-amd64 VHD image for VirtualBox](https://download.freebsd.org/ftp/releases/VM-IMAGES/11.2-RELEASE/amd64/Latest/FreeBSD-11.2-RELEASE-amd64.vhd.xz) and run it on VirtualBox or KVM.
- Please read architecture docs to understand the design goals and install a correct development environment.

- Install Elixir on every node.


## Starting platform
```
mix do clean, deps.clean --all, deps.get, deps.compile, compile, digest
mix phx.server

```

## Building binaries
 NOTE: TODO


## Description
This app has been created by `mix` tool and consists of multiple umbrella apps.

