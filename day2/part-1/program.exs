File.stream!("input.txt")
|> Stream.map(&String.to_charlist/1)
|> Stream.map(fn char_list ->
  first_elf_hand = Enum.at(char_list, 0) - 64
  second_elf_hand = Enum.at(char_list, 2) - 87
  outcome_score = case second_elf_hand - first_elf_hand do
    0 -> 3
    1 -> 6
    -2 -> 6
    _ -> 0
  end
  second_elf_hand + outcome_score
end)
|> Enum.to_list
|> Enum.sum
|> IO.inspect
