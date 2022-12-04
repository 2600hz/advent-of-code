File.stream!("input.txt")
|> Stream.map(&String.to_charlist/1)
|> Stream.map(fn char_list ->
  outcome_score = (Enum.at(char_list, 2) - 88) * 3
  first_elf_hand = Enum.at(char_list, 0) - 65       # 0 = Rock, 1 = Paper, 2 = Scissors
  hand_score = case outcome_score do
    0 -> if first_elf_hand == 0, do: 3, else: first_elf_hand
    3 -> first_elf_hand + 1
    6 -> rem(first_elf_hand + 1, 3) + 1
  end
  outcome_score + hand_score
end)
|> Enum.to_list
|> Enum.sum
|> IO.inspect
