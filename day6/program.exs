defmodule Day6 do
  def read_input do
    File.stream!("input.txt", [], 1)
  end

  def find_marker(marker_size) do
    marker_size + (read_input()
      |> Stream.chunk_every(marker_size, 1, :discard)
      |> Stream.take_while(fn chunk ->
        chunk |> Enum.uniq() |> Enum.count() < marker_size
      end)
      |> Enum.count())
  end

  def part1 do
    find_marker(4)
  end

  def part2 do
    find_marker(14)
  end
end

Day6.part1() |> IO.inspect()

Day6.part2() |> IO.inspect()
