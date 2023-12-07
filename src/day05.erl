-module(day05).

-export([run/0]).

%% https://adventofcode.com/2023/day/5
%% lowest location: 382895070

run() ->
    Almanac = parse_almanac(input("day05_part1.txt")),
    io:format('user', "almanac: ~p~n", [Almanac]),

    part1(Almanac),
    part2(Almanac).

part1({Seeds, Maps}) ->
    SeedRanges = [{Seed, Seed} || Seed <- Seeds],
    Locations = seeds_to_locations({SeedRanges, Maps}),
    io:format('user', "~nlowest location: ~w~n", [lists:min(Locations)]).

seeds_to_locations({Seeds, Maps}) ->
    {_, Locations} = lists:foldl(fun seed_to_location/2, {Maps, []}, Seeds),
    lists:reverse(Locations).

seed_to_location({SeedStart, SeedStart}, {Maps, Locations}) ->
    SrcName = <<"seed">>,
    FinalDstName = <<"location">>,

    Location = seed_to_location(SeedStart, Maps, SrcName, FinalDstName),
    {Maps, [Location | Locations]};
seed_to_location({SeedStart, SeedEnd}, {Maps, Locations}) ->
    {Maps, Ls} = seed_to_location({SeedStart, SeedStart}, {Maps, Locations}),
    seed_to_location({SeedStart+1, SeedEnd}, {Maps, Ls}).

seed_to_location(Seed, Maps, SrcName, FinalDstName) ->
    io:format('user', "Finding ~s~n", [SrcName]),
    seed_to_location(Seed, Maps, SrcName, FinalDstName, lists:keyfind(SrcName, 1, Maps)).

seed_to_location(Seed, _Maps, SrcName, FinalDstName, {SrcName, FinalDstName, Ranges}) ->
    map_src_to_dst(Seed, lists:reverse(Ranges));
seed_to_location(Seed, Maps, SrcName, FinalDstName, {SrcName, DstName, Ranges}) ->
    NewSeed = map_src_to_dst(Seed, lists:reverse(Ranges)),
    seed_to_location(NewSeed, Maps, DstName, FinalDstName).

map_src_to_dst(Seed, []) -> Seed;
map_src_to_dst(SrcStart, [{DstStart, SrcStart, _Range} | _Ranges]) ->
    DstStart;
map_src_to_dst(Seed, [{DstStart, SrcStart, Range} | Ranges]) ->
    case Seed >= SrcStart andalso Seed =< SrcStart+Range of
        'true' ->
            (DstStart - SrcStart) + Seed;
        'false' -> map_src_to_dst(Seed, Ranges)
    end.

parse_almanac(Input) ->
    [SeedsBin | MapBins] = binary:split(Input, <<$\n>>, ['global', 'trim']),
    <<"seeds: ", SeedNoBins/binary>> = SeedsBin,
    SeedNos = [binary_to_integer(SeedNoBin) || SeedNoBin <- binary:split(SeedNoBins, <<" ">>, ['global'])],
    Maps = parse_maps(MapBins),
    {SeedNos, Maps}.

parse_maps(MapBins) ->
    parse_maps(MapBins, []).

parse_maps([], Maps) -> Maps;
parse_maps([<<>>, MapNameBin | MapBins], Maps) ->
    parse_maps([MapNameBin | MapBins], Maps);
parse_maps([MapNameBin | MapBins], Maps) ->
    {'match', [SrcName, DstName]}
        = re:run(MapNameBin
                ,<<"^(\\w+)-to-(\\w+) map:$">>
                ,[{'capture', 'all_but_first', 'binary'}]
                ),
    %% [{srcname, dstname, [range()]}]
    {MapRanges, MapBins1} = parse_map_ranges(MapBins, []),
    parse_maps(MapBins1, [{SrcName, DstName, MapRanges} | Maps]).

parse_map_ranges([], Ranges) -> {Ranges, []};
parse_map_ranges([<<>> | MapBins], Ranges) ->
    {Ranges, MapBins};
parse_map_ranges([SrcDst | MapBins], Ranges) ->
    [DstStartBin, SrcStartBin, RangeBin] = binary:split(SrcDst, <<" ">>, ['global', 'trim']),

    DstStart = binary_to_integer(DstStartBin),
    SrcStart = binary_to_integer(SrcStartBin),
    Range = binary_to_integer(RangeBin),

    parse_map_ranges(MapBins
                    ,[{DstStart, SrcStart, Range} | Ranges]
                    ).

part2({Seeds, Maps}) ->
    SeedRanges = parse_seed_ranges(Seeds),
    Collapsed = collapse_seed_ranges(SeedRanges),

    Locations = find_locations(Collapsed, Maps, []),

    io:format("~nlowest location: ~w~n", [lists:min(Locations)]).

find_locations([], _Maps, Locations) -> Locations;
find_locations([Seed, Range | Seeds], [{_SrcName, _DstName, Ranges} | Maps], Locations) ->
    {Ls, SeedRanges} = find_location(Seed, Seed+Range, Ranges, {Locations, []}),
    find_locations(Seeds ++ SeedRanges, Maps, Ls).

find_location(SeedStart, SeedEnd, [{DstStart, SrcStart, Range} | Maps], {Locations, MoreSeedRanges})
  when SeedStart >= SrcStart andalso SeedEnd =< SrcStart+Range ->
    %% all seed range are mapped to the map's range
    Offset = DstStart - SrcStart,
    find_location(SeedStart + Offset
                 ,SeedEnd + Offset
                 ,Maps
                 ,{Locations, MoreSeedRanges}
                 );
find_location(SeedStart, SeedEnd, [{DstStart, SrcStart, _Range} | Maps], {Locations, MoreSeedRanges})
  when SeedStart < SrcStart andalso SeedEnd > SrcStart ->
    %% seed range
    {Locations, MoreSeedRanges}.



collapse_seed_ranges(SeedRanges) ->
    lists:foldl(fun collapse_seed_range/2, [], SeedRanges).

collapse_seed_range({Start, End}, [{RangeStart, RangeEnd} | Ranges])
  when Start < RangeEnd ->
    %% when Start is within existing range, replace range with new End
    [{RangeStart, End} | Ranges];
collapse_seed_range({Start, End}, Ranges) ->
    [{Start, End} | Ranges].

parse_seed_ranges(Seeds) ->
    parse_seed_ranges(Seeds, []).

parse_seed_ranges([], Ranges) -> lists:keysort(1, Ranges);
parse_seed_ranges([SeedNo, Range | SeedRanges], Ranges) ->
    parse_seed_ranges(SeedRanges, seed_range(SeedNo, Range, Ranges, Ranges)).

seed_range(SeedNo, Range, ExistingSeedRanges, []) ->
    [{SeedNo, SeedNo+Range} | ExistingSeedRanges];
seed_range(SeedNo, Range, ExistingSeedRanges, [{Start, End} | _SeedRanges])
  when SeedNo >= Start andalso SeedNo+Range =< End ->
    %% already have a range covering this seed
    ExistingSeedRanges;
seed_range(SeedNo, Range, ExistingSeedRanges, [{Start, End} | _SeedRanges])
  when SeedNo < Start andalso SeedNo+Range > End  ->
    %% new range covers existing range
    [{SeedNo, SeedNo+Range} | lists:keydelete(Start, 1, ExistingSeedRanges)];
seed_range(SeedNo, Range, ExistingSeedRanges, [{_Start, _End} | SeedRanges]) ->
    seed_range(SeedNo, Range, ExistingSeedRanges, SeedRanges).


input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
