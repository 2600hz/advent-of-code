defmodule Day5 do
  def is_crate(char) do
    char in ?A..?Z
  end

  def build_stacks(initial_stack, file_row) do
    crates = file_row
      |> String.to_charlist()
      |> Enum.chunk_every(4)
      |> Enum.map(fn char_list ->
        Enum.at(char_list, 1)
      end)

    add_to_stacks(initial_stack, crates)
  end

  def add_to_stacks([], []) do
    []
  end

  def add_to_stacks([], char_list) do
    char_list |> Enum.map(fn char ->
      if is_crate(char), do: [char], else: []
    end)
  end

  def add_to_stacks(stacks, []) do
    stacks
  end

  def add_to_stacks([current_stack|remaining_stacks], [current_char|remaining_chars]) do
    new_current_stack = if is_crate(current_char), do: current_stack ++ [current_char], else: current_stack
    [ new_current_stack | add_to_stacks(remaining_stacks, remaining_chars) ]
  end

  def move_between_stacks(stacks, file_row) do
    [{quantity,""},{from,""},{to,""}] = Regex.scan(~r/[[:digit:]]+/, file_row)
      |> List.flatten()
      |> Enum.map(&Integer.parse/1)

    move_crates(quantity, from, to, stacks)
  end

  def take_crates(quantity, 1, [current_stack|stacks]) do
    {taken, remaining} = Enum.split(current_stack, quantity)
    {taken, [remaining | stacks]}
  end

  def take_crates(quantity, from, [current_stack|remaining_stacks]) do
    {taken, updated_remaining_stacks} = take_crates(quantity, from - 1, remaining_stacks)
    {taken, [current_stack|updated_remaining_stacks]}
  end

  def place_crates(crates, 1, [current_stack|remaining_stacks]) do
    [ place_crates_aux(crates, current_stack) | remaining_stacks ]
  end

  def place_crates(crates, to, [current_stack|remaining_stacks]) do
    [ current_stack | place_crates(crates, to - 1, remaining_stacks)]
  end

  def place_crates_aux([], stack) do
    stack
  end

  def place_crates_aux([crate|remaining_crates], stack) do
    place_crates_aux(remaining_crates, [crate|stack])
  end

  def move_crates(quantity, from, to, stacks) do
    {crates, stacks_2} = take_crates(quantity, from, stacks)
    place_crates(crates, to, stacks_2)
  end
end


{_, final_stacks} = File.stream!("input.txt")
  |> Stream.map(&String.trim/1)
  |> Enum.to_list()
  |> Enum.reduce({:build, []}, fn file_row, {action, stacks} ->
    case file_row do
      "" -> {:move, stacks}
      _ -> case action do
        :build -> if String.starts_with?(file_row, " 1") do
            {:build, stacks}
          else
            {:build, Day5.build_stacks(stacks, file_row)}
          end
        _ -> {:move, Day5.move_between_stacks(stacks, file_row)}
      end
    end
  end)

final_stacks |> Enum.map(fn [crate|_] ->
  crate
end) |> IO.inspect()
