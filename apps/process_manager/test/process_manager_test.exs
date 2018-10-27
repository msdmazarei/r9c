defmodule ProcessManagerTest do
  use ExUnit.Case
  doctest ProcessManager

  test "greets the world" do
    assert ProcessManager.hello() == :world
  end
end
