-module(day5).

-export([lowest_location/2]).

lowest_location(Input, Part) ->
    {SeedRanges, Maps} = parse_almanac(Input, Part),
    {Expanded, []} = lists:foldl(fun expand_seed_ranges/2, {SeedRanges, []}, Maps),
    lists:min([Start || {Start, _End} <- Expanded]).

expand_seed_ranges({_Type, _Mappings}, {[], Expanded}) -> {Expanded, []};
expand_seed_ranges({_Type, Mappings}, {[SRange | SRanges], Expanded}) ->
    ExpandedSRanges = expand_seed_range(SRange, Mappings),
    %% Kind of doing a double fold here
    expand_seed_ranges({_Type, Mappings}, {SRanges, ExpandedSRanges ++ Expanded}).

expand_seed_range(SRange, Mappings) -> expand_seed_range([SRange], Mappings, []).

expand_seed_range([], _Mappings, Expanded) -> Expanded;
expand_seed_range(SRanges, [], Expanded) -> SRanges ++ Expanded;
expand_seed_range([SRange | SRanges], [Mapping | Mappings], Expanded) ->
    {ORange, NewSRanges} = partition_overlapping_range(SRange, Mapping),
    Expanded1 = case ORange of
                    'undefined' -> Expanded;
                    {Start, End} -> [{Start, End} | Expanded]
                end,
    expand_seed_range(NewSRanges ++ SRanges, Mappings, Expanded1).

partition_overlapping_range({Start, End}, {SrcStart, SrcEnd, Diff})
  when Start >= SrcStart, End =< SrcEnd ->
    %% Whole range is within mapping
    {{Start + Diff, End + Diff}, []};
partition_overlapping_range({Start, End}, {SrcStart, SrcEnd, Diff})
  when Start < SrcStart, End >= SrcStart, End =< SrcEnd ->
    %% Range overlaps start of mapping
    {{SrcStart + Diff, End + Diff}, [{Start, SrcStart - 1}]};
partition_overlapping_range({Start, End}, {SrcStart, SrcEnd, Diff})
  when Start >= SrcStart, Start =< SrcEnd, End > SrcEnd ->
    %% Range overlaps end of mapping
    {{Start + Diff, SrcEnd + Diff}, [{SrcEnd + 1, End}]};
partition_overlapping_range({Start, End}, {SrcStart, SrcEnd, Diff})
  when Start < SrcStart, End > SrcEnd ->
    %% Range overlaps both ends of mapping
    {{SrcStart + Diff, SrcEnd + Diff}, [{Start, SrcStart - 1}, {SrcEnd + 1, End}]};
partition_overlapping_range({Start, End}, {SrcStart, SrcEnd, _Diff})
  when Start < SrcStart; End > SrcEnd ->
    %% Range is before or after mapping
    {'undefined', [{Start, End}]}.

parse_almanac(Input, Part) ->
    [SeedsInput, MapsInput] = binary:split(Input, <<"\n\n">>),
    {parse_seeds(SeedsInput, Part), parse_maps(MapsInput)}.

parse_seeds(<<"seeds: ", SeedsInput/binary>>, Part) ->
    Seeds = binary:split(SeedsInput, <<" ">>, ['global']),
    parse_seeds(Seeds, Part);
parse_seeds(Seeds, 1) ->
    %% To enable re-use of the code for part 2, we create a list of seed ranges,
    %% each of length 1.
    [{binary_to_integer(Seed), binary_to_integer(Seed)} || Seed <- Seeds];
parse_seeds(Seeds, 2) ->
    parse_seed_ranges(Seeds).

parse_seed_ranges(Seeds) -> parse_seed_ranges(Seeds, []).

parse_seed_ranges([], SRanges) -> SRanges;
parse_seed_ranges([StartBin, LengthBin | Rest], SRanges) ->
    Start = binary_to_integer(StartBin),
    End = binary_to_integer(LengthBin) + Start - 1,
    parse_seed_ranges(Rest, [{Start, End} | SRanges]).

parse_maps(MapsInput) ->
    Maps = binary:split(MapsInput, <<"\n\n">>, ['global']),
    [parse_map(Map) || Map <- Maps].

parse_map(MapInput) ->
    [TypeInput | Mappings] = binary:split(MapInput, <<"\n">>, ['global']),
    {parse_type(TypeInput), [parse_mapping(Mapping) || Mapping <- Mappings]}.

parse_type(TypeInput) -> hd(binary:split(TypeInput, <<" ">>)).

parse_mapping(MappingInput) ->
    Parts = binary:split(MappingInput, <<" ">>, ['global']),
    [DestStart, SrcStart, Length] = [binary_to_integer(Part) || Part <- Parts],
    {SrcStart, SrcStart + Length - 1, DestStart - SrcStart}.
