-module(t1).
-compile(export_all).

t1({B,A}, X) -> {fun t2:t2/2, {X, B, A}}.
trunc(X) -> X.
log(X) -> trunc(X).
