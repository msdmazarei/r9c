defmodule HttpServersTest do
  use ExUnit.Case
  doctest HttpServers

  test "greets the world" do
    assert HttpServers.hello() == :world
  end
end
