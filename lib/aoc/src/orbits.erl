-module(orbits).

-export([from_binary/1]).

-export([total_orbits/1, total_orbits/2
        ,orbital_transfers/3
        ]).

-type orbits() :: digraph:graph().

-spec total_orbits(orbits(), binary()) -> non_neg_integer().
total_orbits(OrbitData, Object) ->
    case digraph:get_short_path(OrbitData, <<"COM">>, Object) of
        'false' -> 0;
        [<<"COM">> | Path] -> length(Path)
    end.

-spec total_orbits(orbits()) -> non_neg_integer().
total_orbits(OrbitData) ->
    lists:sum([total_orbits(OrbitData, Object) || Object <- digraph:vertices(OrbitData)]).

orbital_transfers(OrbitData, From, To) ->
    PathFrom = digraph:get_short_path(OrbitData, <<"COM">>, From),
    PathTo = digraph:get_short_path(OrbitData, <<"COM">>, To),

    Path = find_intersection(PathFrom, PathTo),
    length(Path -- [From, To]) - 1.

find_intersection([A, B | From], [A, B | To]) ->
    find_intersection([B | From], [B | To]);
find_intersection([A | _]=From, [A | To]) ->
    lists:reverse(From) ++ To.

-spec from_binary(binary()) -> orbits().
from_binary(Contents) ->
    OrbitBins = binary:split(Contents, <<"\n">>, ['global']),
    lists:foldl(fun binary_to_orbit/2, digraph:new(['acyclic']), OrbitBins).

binary_to_orbit(<<>>, Graph) -> Graph;
binary_to_orbit(OrbitBin, Graph) ->
    [COM, SAT] = binary:split(OrbitBin, <<")">>),
    V1 = digraph:add_vertex(Graph, COM),
    V2 = digraph:add_vertex(Graph, SAT),
    _Edge = digraph:add_edge(Graph, V1, V2, OrbitBin),
    Graph.
