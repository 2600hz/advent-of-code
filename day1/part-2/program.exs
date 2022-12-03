chunk_fun = fn element, acc ->
  if (element == "") do
    {:cont, acc, 0}
  else
    {:cont, (Integer.parse(element) |> Kernel.elem(0)) + acc}
  end
end

after_fun = fn
  0 -> {:cont, 0}
  acc -> {:cont, acc, 0}
end

File.stream!("input.txt")
|> Stream.map(&String.trim/1)
|> Stream.chunk_while(0, chunk_fun, after_fun)
|> Enum.to_list
|> Enum.sort
|> Enum.take(-3)
|> Enum.sum
|> IO.inspect

#Enum.to_list(stream)
