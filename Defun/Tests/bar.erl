-module(bar).
-compile(export_all).

%g() -> "ab\"cde" ++ "".

task
t(0) -> task(N) -> N + 1 end;
t(X) -> A=1, B=2, 
        task(X) -> task(Y) -> log(X) + Y * A * B end end.

log(X) -> trunc(X).
trunc(X) -> X.

ceiling(X) when X > trunc(X) -> trunc(X) + 1.0;
ceiling(X) -> trunc(X).

log2(X) -> log(X) / log(2) + X(X).

exp2(0) -> 1;
exp2(N) -> E = exp2(N div 2), (1 + N rem 2) * (E * E).

rho(1) -> 0;
rho(N) -> case N rem 2 of
            0 -> rho(N div 2) + 1;
            1 -> 0 + foo(N+1)
          end.

foo (0) -> 0;
foo (N) -> rho(N-1).

nu(0) -> 0;
nu(N) -> nu(N div 2) + N rem 2.

bms(0) -> 0;
bms(1) -> 0;
bms(N) ->
    K = N div 2,
    case N rem 2 of
      0 -> bms(N - 1) + nu(K - 1) + 1;
      1 -> bms(N - 2) + 2 * exp2(rho(K)) + nu(K - 1) + nu(K)
    end.

tms(0) -> 0;
tms(1) -> 0;
tms(N) -> L = ceiling(log2(N)), N * L - exp2(L) + 1.

map(F,   []) -> [];
map(F,[X|S]) -> [F(X) | map(F,S)].

f(F,X) -> (if X -> F; true -> fun f/1 end)(X).

g(X) -> if X -> hi; true -> bye end.

h(X) -> case X of true -> hi; _ -> bye end.

i(X) -> (fun tms/1)(X).

