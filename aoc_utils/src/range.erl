-module(range).

-export([partition_overlap/2
        ,overlap/2
        ]).

partition_overlap(R1, R2) ->
    {overlap_before(R1, R2), overlap(R1, R2), overlap_after(R1, R2)}.

overlap_before({Start1, End1}, {Start2, _End2}) when Start1 < Start2 ->
    {Start1, min(End1, Start2 - 1)};
overlap_before({Start1, _End1}, {Start2, End2}) when Start1 > Start2 ->
    {Start2, min(End2, Start1 - 1)};
overlap_before({Start1, _End1}, {Start2, _End2}) when Start1 == Start2 ->
    'undefined'.

overlap({Start1, End1}, {Start2, End2}) when End1 < Start2; End2 < Start1 ->
    'undefined';
overlap({Start1, End1}, {Start2, End2}) when Start1 < Start2 ->
    {Start2, min(End1, End2)};
overlap({Start1, End1}, {Start2, End2}) when Start1 >= Start2 ->
    {Start1, min(End1, End2)}.

overlap_after({_Start1, End1}, {_Start2, End2}) when End1 < End2 ->
    {End1 + 1, End2};
overlap_after({_Start1, End1}, {_Start2, End2}) when End1 > End2 ->
    {End2 + 1, End1};
overlap_after({_Start1, End1}, {_Start2, End2}) when End1 == End2 ->
    'undefined'.
