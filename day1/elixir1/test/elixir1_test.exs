defmodule Elixir1Test do
  use ExUnit.Case
  doctest Elixir1

  test "greets the world" do
    assert Elixir1.hello() == :world
  end
end
