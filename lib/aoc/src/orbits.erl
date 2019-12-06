-module(orbits).

-export([from_binary/1]).

-export([total_orbits/1, total_orbits/2]).

%% {graph of orbits, object list}
-type orbits() :: {digraph:graph(), [binary()]}.

-spec total_orbits(orbits()) -> non_neg_integer().
total_orbits({OrbitData, _Objects}, Object) ->
    case digraph:get_short_path(OrbitData, <<"COM">>, Object) of
        'false' -> 0;
        [<<"COM">> | Path] -> length(Path)
    end.

total_orbits({_OrbitData, Objects}=Orbits) ->
    lists:sum([total_orbits(Orbits, Object) || Object <- Objects]).

-spec from_binary(binary()) -> orbits().
from_binary(Contents) ->
    OrbitBins = binary:split(Contents, <<"\n">>, ['global']),
    {Graph, Objects} = lists:foldl(fun binary_to_orbit/2, {digraph:new(['acyclic']), []}, OrbitBins),
    {Graph, lists:usort(Objects)}.

binary_to_orbit(<<>>, Acc) -> Acc;
binary_to_orbit(OrbitBin, {Graph, Objects}) ->
    [COM, SAT] = binary:split(OrbitBin, <<")">>),
    V1 = digraph:add_vertex(Graph, COM),
    V2 = digraph:add_vertex(Graph, SAT),
    _Edge = digraph:add_edge(Graph, V1, V2, OrbitBin),
    {Graph, [COM, SAT | Objects]}.
