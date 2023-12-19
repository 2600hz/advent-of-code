-module(math2).

-export([quadratic/4]).

quadratic(A, B, C, UpperOrLower) ->
    Oper = case UpperOrLower of
               'upper' -> fun 'erlang':'+'/2;
               'lower' -> fun 'erlang':'-'/2
           end,
    Oper(-B, math:sqrt(math:pow(B, 2) - 4 * A * C)) / (2 * A).
