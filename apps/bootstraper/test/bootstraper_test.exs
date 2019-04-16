defmodule BootstraperTest do
  use ExUnit.Case
  doctest Bootstraper

  test "greets the world" do
    assert Bootstraper.hello() == :world
  end
end
