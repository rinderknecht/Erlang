-module(def4).
-compile(export_all).

f(X) -> fun(Y) -> X + Y end.
g() -> (h(f(1)))(2).
h(F) -> F.
i() -> h(fun() -> 3 end).
j() -> h(fun g/0).
k(A, B) -> A + B.
l(F, X) -> F(k(1,X)).
m() -> l(fun k/2, 5).
