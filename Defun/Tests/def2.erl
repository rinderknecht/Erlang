-module(def2).
-compile(export_all).

map(F,[]) -> [];
map(F,[X|S]) -> [F(X)|map(F,S)].

main() -> map(fun(N) -> N+1 end, [0,1,2]).
