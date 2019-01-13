-module(qual).
-compile(export_all).

f(E) -> (element(1,E)):(g())().

g() -> a.
