defmodule DatabaseEngineTest do
  use ExUnit.Case, async: true

  doctest DatabaseEngine

  describe "basic stuff" do
    test "greets the world" do
      assert DatabaseEngine.hello() == :world
    end
  end
end
