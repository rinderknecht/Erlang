-module(danvy2).
-compile(export_all).

aux(I,F) -> F(I).

walk(I,[])     -> [];
walk(I,[J|Js]) -> [aux (I, fun(I) -> I+J end) | walk(I,Js)].
