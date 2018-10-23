defmodule DatabaseEngineTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  doctest DatabaseEngine

  test "greets the world" do
    assert DatabaseEngine.hello() == :world
  end
end
