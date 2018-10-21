defmodule GatewayCoreTest do
  use ExUnit.Case
  doctest GatewayCore

  test "greets the world" do
    assert GatewayCore.hello() == :world
  end
end
