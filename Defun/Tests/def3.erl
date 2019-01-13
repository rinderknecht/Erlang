-module(def3).
-compile(export_all).

f(X) -> fun(Y) -> fun() -> X + Y end end.
g() -> h((f(1))(2)).
h(F) -> F().
i() -> h(fun() -> 3 end).
j() -> h(fun foo:g/0).
k(A,B) -> A + B.
l(F,X) -> F(1,X).
m() -> l(fun k/2,5).

