-module(foo).
-compile(export_all).
trunc(X) -> X.
log(X) -> trunc(X).
task
t(0) -> {task t3:t3/2, {}};
t(X) -> A=1, B=2, {task t1:t1/2, {B, A}}.
