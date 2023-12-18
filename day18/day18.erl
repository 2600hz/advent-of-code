-module(day18).

-export([lava_volume/2]).

lava_volume(Input, Part) ->
    Plan = parse_plan(Input),
    Plan1 = case Part of
                1 -> Plan;
                2 -> decode_plan(Plan)
            end,
    Coords = plan_to_polygon(Plan1),
    Area = polygon_area(Coords),
    BoundaryPointsCount = perimeter(Coords),
    interior_points_count(floor(Area), BoundaryPointsCount) + BoundaryPointsCount.

plan_to_polygon(Plan) ->
    {_End, Vertices} = lists:foldl(fun dig/2, {{0, 0}, []}, Plan),
    lists:reverse(Vertices).

dig({Dir, Dist, _Color}, {{I, J}, Coords}) ->
    {I1, J1} = case Dir of
                   'u' -> {I - Dist, J};
                   'd' -> {I + Dist, J};
                   'l' -> {I, J - Dist};
                   'r' -> {I, J + Dist}
               end,
    {{I1, J1}, [{I1, J1} | Coords]}.

perimeter(Vertices) -> perimeter(Vertices, hd(Vertices), 0).

perimeter([{I1, J1}], {I2, J2}, Perimeter) ->
    Perimeter + manhattan_distance({I1, J1}, {I2, J2});
perimeter([{I1, J1}, {I2, J2} | Vertices], Start, Perimeter) ->
    Dist = manhattan_distance({I1, J1}, {I2, J2}),
    perimeter([{I2, J2} | Vertices], Start, Perimeter + Dist).

polygon_area(Coords) ->
    polygon_area(Coords, hd(Coords), 0).

polygon_area([{Y1, X1}], {Y2, X2}, Area) ->
    TriangleArea = triangle_area({Y1, X1}, {Y2, X2}),
    abs(Area + TriangleArea);
polygon_area([{Y1, X1}, {Y2, X2} | Coords], Start, Area) ->
    TriangleArea = triangle_area({Y1, X1}, {Y2, X2}),
    polygon_area([{Y2, X2} | Coords], Start, Area + TriangleArea).

triangle_area({Y1, X1}, {Y2, X2}) -> (X1 * Y2 - X2 * Y1) / 2.

interior_points_count(Area, BoundaryPointsCount) ->
    Area - BoundaryPointsCount div 2 + 1.

manhattan_distance({I1, J1}, {I2, J2}) -> abs(I2 - I1) + abs(J1 - J2).

parse_plan(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [Dir, Dist, Color] = binary:split(Line, <<" ">>, ['global']),
    {parse_direction(Dir), binary_to_integer(Dist), parse_color(Color)}.

parse_direction(<<"U">>) -> 'u';
parse_direction(<<"D">>) -> 'd';
parse_direction(<<"L">>) -> 'l';
parse_direction(<<"R">>) -> 'r'.

parse_color(<<"(#", Color:6/binary, ")">>) -> Color.

decode_plan(Plan) -> [decode_line(Line) || Line <- Plan].

decode_line({_Dir, _Dist, <<DistEncoded:5/binary, DirEncoded:8>>=Color}) ->
    Dir = decode_direction(DirEncoded),
    Dist = binary_to_integer(DistEncoded, 16),
    {Dir, Dist, Color}.

decode_direction($0) -> 'r';
decode_direction($1) -> 'd';
decode_direction($2) -> 'l';
decode_direction($3) -> 'u'.
