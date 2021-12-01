#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

%% As the submarine drops below the surface of the ocean, it
%% automatically performs a sonar sweep of the nearby sea floor. On a
%% small screen, the sonar sweep report (your puzzle input) appears:
%% each line is a measurement of the sea floor depth as the sweep
%% looks further and further away from the submarine.

%% For example, suppose you had the following report:

%% 199
%% 200
%% 208
%% 210
%% 200
%% 207
%% 240
%% 269
%% 260
%% 263

%% This report indicates that, scanning outward from the submarine,
%% the sonar sweep found depths of 199, 200, 208, 210, and so on.

%% The first order of business is to figure out how quickly the depth
%% increases, just so you know what you're dealing with - you never
%% know if the keys will get carried into deeper water by an ocean
%% current or a fish or something.

%% To do this, count the number of times a depth measurement increases
%% from the previous measurement. (There is no measurement before the
%% first measurement.) In the example above, the changes are as
%% follows:

%% 199 (N/A - no previous measurement)
%% 200 (increased)
%% 208 (increased)
%% 210 (increased)
%% 200 (decreased)
%% 207 (increased)
%% 240 (increased)
%% 269 (increased)
%% 260 (decreased)
%% 263 (increased)

%% In this example, there are 7 measurements that are larger than the
%% previous measurement.

%% How many measurements are larger than the previous measurement?

-mode(compile).

main(_) ->
    [First | Measurements] = read_input("p1.txt"),
    Count = count_incr(First, Measurements, 0),
    io:format("depth increased ~p times~n", [Count]).

count_incr(_CurrentDepth, [], Count) -> Count;
count_incr(CurrentDepth, [NextDepth | Depths], Count) when NextDepth > CurrentDepth ->
    count_incr(NextDepth, Depths, Count+1);
count_incr(_CurrentDepth, [NextDepth | Depths], Count) ->
    count_incr(NextDepth, Depths, Count).

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [binary_to_integer(Line, 10) || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].
