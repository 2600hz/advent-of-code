-module(math2).

-export([lcm/2
        ,gcd/2
        ,quadratic/4
        ,newton_interpolation_polynomial/2
        ,newton_basis_polynomial/2
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

%% https://en.wikipedia.org/wiki/Newton_polynomial
newton_interpolation_polynomial(X, [Y0 | _]=Ys) ->
    newton_interpolation_polynomial(
      X
     ,Ys
     ,1
      %% Coefficient `y0' is used as the initial value
     ,Y0
      %% Previous `yb' for `y0' can just be 0
     ,[0]
     ).

newton_interpolation_polynomial(_, [_], _, N, _) -> N;
newton_interpolation_polynomial(X, [Y0, Y1 | Ys], K, N, [YB0 | YBs]) ->
    %% `x' data point interval of 1 simplifies these next 2 equations
    YB = (Y1 - Y0 - YB0) div K,
    N1 = N + YB * newton_basis_polynomial(X, K),
    newton_interpolation_polynomial(X, [Y1 | Ys], K + 1, N1, [YB, YB0 | YBs]).

newton_basis_polynomial(X, J) -> newton_basis_polynomial(0, X, J, 1).

newton_basis_polynomial(J, _, J, Acc) -> Acc;
newton_basis_polynomial(I, X, J, Acc) ->
    newton_basis_polynomial(I + 1, X, J, Acc * (X - I)).
