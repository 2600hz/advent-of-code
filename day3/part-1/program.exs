File.stream!("input.txt")
|> Stream.map(&String.trim/1)
|> Stream.map(&String.to_charlist/1)
|> Stream.map(fn char_list ->
  half_length = round(length(char_list) / 2)
  {a, b} = char_list |> Enum.split(half_length)
  set_a = MapSet.new(a)
  set_b = MapSet.new(b)
  intersection = MapSet.intersection(set_a, set_b)
  intersection
  |> Enum.map(fn char ->
    if char >= 97, do: char - 97 + 1, else: char - 65 + 27
  end)
  |> Enum.sum
end)
|> Enum.to_list
|> Enum.sum
|> IO.inspect
