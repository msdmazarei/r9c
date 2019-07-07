defmodule EventLoggerTest do
  use ExUnit.Case
  doctest EventLogger

  test "greets the world" do
    assert EventLogger.hello() == :world
  end
end
