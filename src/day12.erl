-module(day12).

-export([run/0]).

%% https://adventofcode.com/2023/day/12
%% sum of arrangements: 6981

run() ->
    Input = parse_reports(input("day12.txt")),
    part1(Input),
    part2(Input).

part1(Reports) ->
    Arrs = count_arrangements(Reports),
    io:format("sum of arrangements: ~p~n", [lists:sum(Arrs)]).

count_arrangements(Reports) ->
    [count_arrangement(Report) || Report <- Reports].

count_arrangement({SC, DR}) ->
    Options = enumerate_options(SC),
    lists:foldl(fun(O, C) ->
                        case damage_report(O) =:= DR of
                            'true' -> C+1;
                            'false' -> C
                        end
                end
               ,0
               ,Options
               ).

enumerate_options([$?]) -> [[$.], [$#]];
enumerate_options([$.]) -> [[$.]];
enumerate_options([$#]) -> [[$#]];

enumerate_options([$? | Conditions]) ->
    Options = enumerate_options(Conditions),
    [[$. | Option] || Option <- Options]
        ++ [[$# | Option] || Option <- Options];
enumerate_options([C | Conditions]) ->
    Options = enumerate_options(Conditions),
    [[C | Option] || Option <- Options].

%% take a spring condition report and generate the damaged springs report
damage_report(SpringConditions) ->
    case lists:foldl(fun damage_report_fold/2, {0, []}, SpringConditions) of
        {0, RevReport} -> lists:reverse(RevReport);
        {Damaged, RevReport} -> lists:reverse([Damaged | RevReport])
    end.

damage_report_fold($#, {Damaged, Report}) ->
    {Damaged+1, Report};
damage_report_fold($., {0, Report}) -> {0, Report};
damage_report_fold($., {Damaged, Report}) ->
    {0, [Damaged | Report]}.


part2(Input) ->
    Input.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_reports(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [SC, DR] = binary:split(Line, <<" ">>),
    {binary_to_list(SC), [binary_to_integer(I) || I <- binary:split(DR, <<",">>, ['global'])]}.
