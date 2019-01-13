-module(a).
-compile(export_all).

f() -> A=fun () -> bar end, A(1).
