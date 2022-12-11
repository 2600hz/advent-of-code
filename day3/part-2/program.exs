File.stream!("input.txt")
|> Stream.map(&String.trim/1)
|> Stream.map(&String.to_charlist/1)
|> Stream.map(fn char_list ->
  char_list
  |> Enum.map(fn char ->
    if char >= 97, do: char - 97 + 1, else: char - 65 + 27
  end)
  |> MapSet.new()
end)
|> Stream.chunk_every(3)
|> Stream.map(fn priority_set ->
  priority_set
  |> Enum.reduce(fn set, acc ->
    set |> MapSet.intersection(acc)
  end)
end)
|> Stream.flat_map(&Function.identity/1)
|> Enum.sum()
|> IO.inspect
