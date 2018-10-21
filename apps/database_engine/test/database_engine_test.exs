defmodule DatabaseEngineTest do
  use ExUnit.Case
  doctest DatabaseEngine

  test "greets the world" do
    assert DatabaseEngine.hello() == :world
  end
end
