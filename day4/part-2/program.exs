File.stream!("input.txt")
|> Stream.map(fn line ->
  [{n1,""},{n2,""},{n3,""},{n4,""}] = Regex.scan(~r/[[:digit:]]+/, line)
    |> List.flatten()
    |> Enum.map(&Integer.parse/1)
  if (n1 >= n3 && n1 <= n4) || (n2 >= n3 && n2 <= n4) || (n3 >= n1 && n3 <= n2) || (n4 >= n1 && n4 <= n2) do
    1
  else
    0
  end
end)
|> Enum.sum()
|> IO.inspect
