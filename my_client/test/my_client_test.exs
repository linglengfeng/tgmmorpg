defmodule MyClientTest do
  use ExUnit.Case
  doctest MyClient

  test "greets the world" do
    assert MyClient.hello() == :world
  end
end
