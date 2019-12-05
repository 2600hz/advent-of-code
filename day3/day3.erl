-module(day3).
-export([find_closest_intersection/0, fewest_steps_to_intersection/0]).

read_input(InputFile) ->
    {'ok', Input} = file:read_file(InputFile),
    binary:part(Input, {0, byte_size(Input) - 1}).

process_input(Input) ->
    [Binary1, Binary2] = binary:split(Input, <<"\n">>),
    {binary:split(Binary1, <<",">>, ['global']), binary:split(Binary2, <<",">>, ['global'])}.

binary_to_direction(<<"U">>) -> up;
binary_to_direction(<<"D">>) -> down;
binary_to_direction(<<"L">>) -> left;
binary_to_direction(<<"R">>) -> right.

binary_to_wiremap(L) ->
    binary_to_wiremap(L, []).

binary_to_wiremap([H | T], WireMap) ->
    <<BinaryDirection:1/binary, BinaryDistance/binary>> = H,
    binary_to_wiremap(T, [{binary_to_direction(BinaryDirection), binary_to_integer(BinaryDistance)} | WireMap]);
binary_to_wiremap([], Directions) ->
    lists:reverse(Directions).

wiremap_to_wire_plot(L) ->
    wiremap_to_wire_plot(L, [{0, 0}], {0, 0}).

wiremap_to_wire_plot([{Direction, Distance} | T], Points, CurrentPoint) ->
    {NewSegment, LastPoint} = add_line_segment(Direction, Distance, CurrentPoint, []),
    wiremap_to_wire_plot(T, [NewSegment | Points], LastPoint);
wiremap_to_wire_plot([], Points, _) ->
    lists:flatten(lists:reverse(Points)).

add_line_segment(_, 0, CurrentPoint, Points) ->
    {lists:reverse(Points), CurrentPoint};
add_line_segment('up', Distance, {X, Y}, Points) ->
    add_line_segment('up', Distance - 1, {X, Y + 1}, [{X, Y + 1} | Points]);
add_line_segment('down', Distance, {X, Y}, Points) ->
    add_line_segment('down', Distance - 1, {X, Y - 1}, [{X, Y - 1} | Points]);
add_line_segment('left', Distance, {X, Y}, Points) ->
    add_line_segment('left', Distance - 1, {X - 1, Y}, [{X - 1, Y} | Points]);
add_line_segment('right', Distance, {X, Y}, Points) ->
    add_line_segment('right', Distance - 1, {X + 1, Y}, [{X + 1, Y} | Points]).

find_intersections(L1, L2) ->
    sets:to_list(sets:intersection(sets:from_list(L1), sets:from_list(L2))).

manhatten_distance({X, Y}) ->
    abs(X) + abs(Y).

closest_intersection([H | T]) when H =:= {0,0} ->
    closest_intersection(T);
closest_intersection([H | T]) ->
    closest_intersection(T, manhatten_distance(H)).

closest_intersection([H | T], ShortestDistance) ->
    ManhattenDistance = manhatten_distance(H),
    case ManhattenDistance < ShortestDistance of
        false -> closest_intersection(T, ShortestDistance);
        true  -> closest_intersection(T, ManhattenDistance)
    end;
closest_intersection([], ShortestDistance) ->
    ShortestDistance.

steps_to_intersection([Intersection | _], Intersection) ->
    0;
steps_to_intersection([_ | T], Intersection) ->
    steps_to_intersection(T, Intersection, 1).

steps_to_intersection([Intersection | _], Intersection, Steps) ->
    Steps;
steps_to_intersection([_ | T], Intersection, Steps) ->
    steps_to_intersection(T, Intersection, Steps + 1);
steps_to_intersection([], _, _) ->
    false.

least_steps(WirePlot1, WirePlot2, [H | T]) ->
    ShortestDistance = steps_to_intersection(WirePlot1, H) + steps_to_intersection(WirePlot2, H),
    least_steps(WirePlot1, WirePlot2, T, ShortestDistance).

least_steps(WirePlot1, WirePlot2, [H | T], ShortestDistance) ->
    Distance = steps_to_intersection(WirePlot1, H) + steps_to_intersection(WirePlot2, H),
    case Distance < ShortestDistance of
        false -> least_steps(WirePlot1, WirePlot2, T, ShortestDistance);
        true  -> least_steps(WirePlot1, WirePlot2, T, Distance)
    end;
least_steps(_, _, [], ShortestDistance) ->
    ShortestDistance.

find_closest_intersection() ->
    {Binary1, Binary2} = process_input(read_input("day3.input")),
    WireMap1 = binary_to_wiremap(Binary1),
    WireMap2 = binary_to_wiremap(Binary2),
    WirePlot1 = wiremap_to_wire_plot(WireMap1),
    WirePlot2 = wiremap_to_wire_plot(WireMap2),
    Intersections = find_intersections(WirePlot1, WirePlot2),
    closest_intersection(Intersections).

fewest_steps_to_intersection() ->
    {Binary1, Binary2} = process_input(read_input("day3.input")),
    WireMap1 = binary_to_wiremap(Binary1),
    WireMap2 = binary_to_wiremap(Binary2),
    WirePlot1 = wiremap_to_wire_plot(WireMap1),
    WirePlot2 = wiremap_to_wire_plot(WireMap2),
    Intersections = find_intersections(WirePlot1, WirePlot2),
    NewIntersections = lists:delete({0,0}, Intersections),
    least_steps(WirePlot1, WirePlot2, NewIntersections).
