-module(t2).
-compile(export_all).

t2({X,B,A}, Y) -> log(X) + Y * A * B.
trunc(X) -> X.
log(X) -> trunc(X).
