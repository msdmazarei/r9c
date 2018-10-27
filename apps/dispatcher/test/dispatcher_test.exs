defmodule DispatcherTest do
  use ExUnit.Case
  doctest Dispatcher

  test "greets the world" do
    assert Dispatcher.hello() == :world
  end
end
