-module(def1).
-compile(export_all).

i(X) -> (fun tms/1)(X).
j(X,Y) -> bms(X,Y).
k(F,Z) -> (if Z -> F; true -> fun tms/1 end)(Z).
