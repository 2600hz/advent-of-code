-module(day13).

-export([summarize_notes/2
        ,repr_pattern/1
        ,bits_set/1
        ]).

summarize_notes(Input, Part) ->
    Patterns = parse_patterns(Input),
    Mapper = case Part of
                 1 -> fun reflection/1;
                 2 -> fun smudged_reflection/1
             end,
    Reflections = [Mapper(Pattern) || Pattern <- Patterns],
    Summaries = [summary(Reflection) || Reflection <- Reflections],
    lists:sum(Summaries).

reflection(Pattern) ->
    %% No excluded reflections for part 1
    reflection(Pattern, 'undefined').

reflection(Pattern, ExcludedReflection) ->
    case reflection(Pattern, ExcludedReflection, 'horizontal', 0) of
        'undefined' ->
            Pattern1 = transpose(Pattern),
            reflection(Pattern1, ExcludedReflection, 'vertical', 0);
        Reflection -> Reflection
    end.

smudged_reflection(Pattern) ->
    %% READ THE INSTRUCTIONS CAREFULLY
    %% Even if a bit is flipped, the original reflection might still be valid
    %% and the instructions say to return a **different reflection line**.
    ExcludedReflection = reflection(Pattern),
    case smudged_reflection(Pattern, ExcludedReflection) of
        'undefined' ->
            Pattern1 = transpose(Pattern),
            %% Directions are reversed when transposing
            ExcludedReflection1 = reverse_direction(ExcludedReflection),
            reverse_direction(smudged_reflection(Pattern1, ExcludedReflection1));
        Reflection -> Reflection
    end.

smudged_reflection(Pattern, ExcludedReflection) ->
    smudged_reflection(Pattern, Pattern, ExcludedReflection, []).

smudged_reflection([], _Bs, _ExcludedReflection, _Prefix) -> 'undefined';
smudged_reflection([A | As], [], ExcludedReflection, Prefix) ->
    %% Out of B bitstrings, start again from the top but from the next bitstring
    %% in As
    smudged_reflection(As, lists:reverse(Prefix) ++ As, ExcludedReflection, [A | Prefix]);
smudged_reflection([A | AR]=As, [B | Bs], ExcludedReflection, Prefix) ->
    SizeA = bit_size(A),
    SizeB = bit_size(B),
    <<A1:SizeA>> = A,
    <<B1:SizeB>> = B,
    XOR = A1 bxor B1,
    case bits_set(XOR) of
        1 ->
            %% Only one bit is flipped, so we can check if it's the smudge
            Pattern = lists:reverse(Prefix) ++ [<<B1:SizeB>> | AR],
            maybe_smudged_reflection(Pattern, ExcludedReflection, As, Bs, Prefix);
        _ -> smudged_reflection(As, Bs, ExcludedReflection, Prefix)
    end.

maybe_smudged_reflection(Pattern, ExcludedReflection, As, Bs, Prefix) ->
    case reflection(Pattern, ExcludedReflection) of
        Reflection when Reflection =/= 'undefined',
                        Reflection =/= ExcludedReflection ->
            Reflection;
        _ -> smudged_reflection(As, Bs, ExcludedReflection, Prefix)
    end.

reflection(Pattern, _ExcludedReflection, _Dir, N) when N =:= length(Pattern) ->
    'undefined';
reflection(Pattern, ExcludedReflection, Dir, N) ->
    {Before, After} = lists:split(N, Pattern),
    %% Partition at N, reverse the first part and check if it's a reflection
    case is_reflection(lists:reverse(Before), After) of
        'true' when {Dir, N} =/= ExcludedReflection -> {Dir, N};
        _IsReflection -> reflection(Pattern, ExcludedReflection, Dir, N + 1)
    end.

is_reflection(Before, After) ->
    MinLength = min(length(Before), length(After)),
    Before1 = lists:sublist(Before, MinLength),
    After1 = lists:sublist(After, MinLength),
    Before1 == After1
        andalso length(Before1) > 0
        andalso length(After1) > 0.

transpose(Pattern) -> transpose(Pattern, []).

transpose([], Acc) -> Acc;
transpose([Line | Lines], Acc) ->
    transpose(Lines, transpose_line(Line, Acc)).

transpose_line(Line, LAcc) -> transpose_line(Line, LAcc, []).

transpose_line(<<>>, _LAcc, Acc) -> lists:reverse(Acc);
transpose_line(Line, [], Acc) -> transpose_line(Line, [<<>>], Acc);
transpose_line(<<C:1, Rest/bitstring>>, [H | T], Acc) ->
    transpose_line(Rest, T, [<<H/bitstring, C:1>> | Acc]).

summary({'horizontal', N}) -> 100 * N;
summary({'vertical', N}) -> N.

reverse_direction({'horizontal', N}) -> {'vertical', N};
reverse_direction({'vertical', N}) -> {'horizontal', N}.

parse_patterns(Input) ->
    Patterns = binary:split(Input, <<"\n\n">>, ['global']),
    [parse_pattern(Pattern) || Pattern <- Patterns].

parse_pattern(Pattern) ->
    Lines = binary:split(Pattern, <<"\n">>, ['global']),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    << <<(symbol_bit(C)):1>> || <<C>> <= Line>>.

repr_pattern(Pattern) ->
    [repr_line(Line) || Line <- Pattern].

repr_line(Line) ->
    << <<(bit_symbol(C))>> || <<C:1>> <= Line>>.

symbol_bit($#) -> 1;
symbol_bit($.) -> 0.

bit_symbol(1) -> $#;
bit_symbol(0) -> $..

bits_set(N) -> bits_set(N, 0).

bits_set(0, Acc) -> Acc;
bits_set(N, Acc) ->
    bits_set(N bsr 1, Acc + (N band 1)).
