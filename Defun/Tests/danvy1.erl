-module(danvy1).
-compile(export_all).

aux(F) -> F(1) + F(10).

main(X,Y,B) -> aux(fun(Z) -> X + Z end) *
               aux(fun(Z) -> if B -> Y+Z; true -> Y-Z end end).
