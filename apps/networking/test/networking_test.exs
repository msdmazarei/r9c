defmodule NetworkingTest do
  use ExUnit.Case
  doctest Networking

  test "greets the world" do
    assert Networking.hello() == :world
  end
end
