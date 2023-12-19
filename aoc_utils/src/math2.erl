-module(math2).

-export([lcm/2
        ,gcd/2
        ,quadratic/4
        ]).

lcm(A, B) -> A * B div gcd(A, B).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

quadratic(A, B, C, UpperOrLower) ->
    Oper = case UpperOrLower of
               'upper' -> fun 'erlang':'+'/2;
               'lower' -> fun 'erlang':'-'/2
           end,
    Oper(-B, math:sqrt(math:pow(B, 2) - 4 * A * C)) / (2 * A).
