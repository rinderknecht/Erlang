-module(len).
-compile(export_all).

f(S) ->
  G = fun(F) -> fun(   [],N) -> N;
                   ([_|T],N) -> F(T,N+1)
                end
      end,
  H = fun(F) -> G(fun(X,Y) -> (F(F))(X,Y) end) end,
  (H(H))(S,0).
