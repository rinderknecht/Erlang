-module(len_expected).
-compile(export_all).

app({len,tag4,[]},[F]) -> {len,tag3,F};
app({len,tag3,[F]},[[],N]) -> N;
app({len,tag3,[F]},[[_|T],N]) ->
    X1=(element(1,F)):app(element(2,F),[T,N+1]),
    X1;
app({len,tag2,[G]},[F]) ->
    X1=(element(1,G)):app(element(2,G),[{len,tag1,[F]}]),
    X1;
app({len,tag1,[F]},[X,Y]) ->
    X1=(element(1,F)):app(element(2,F),[F]),
    X2=(element(1,X1)):app(element(2,X1),[X,Y]),
    X2.

len4(S) ->
    G={len,tag4,[]},
    H={len,tag2,[G]},
    X1=(element(1,H)):app(element(2,H),[H]),
    X2=(element(1,X1)):app(element(2,X1),[S,0]),
    X2.
