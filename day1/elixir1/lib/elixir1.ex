defmodule Elixir1 do
  def readFile(file) do
    case File.read(file) do
      {:ok, body} ->
        input = Enum.drop Enum.map(String.codepoints(body), fn(x) -> Integer.parse(x) end), -1
        intList = Enum.map(input, fn(x) -> elem(x, 0) end)
        total = get_pairs(intList, 0)
        add_first_and_last(intList, total)
      {:error, reason} -> reason
    end
  end

  def get_pairs([h | [h2 | t]], accumulator) when h !== h2 do
    get_pairs([h2 | t], accumulator)
  end

  def get_pairs([h | [h2 | t]], accumulator) when h == h2 do
    get_pairs([h2 | t], accumulator + h)
  end

  def get_pairs(_, accumulator) do
    accumulator
  end

  def add_first_and_last([h | t], total) do
    if h == List.last(t) do
      total + h
    else
      total
    end
  end

  def main() do
    file = "../day1Puzzle.txt"
    readFile(file)
  end
end
