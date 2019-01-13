-module(len_out).
-compile(export_all).
app({tag4,[G]}, [F]) -> V4=(element(1,G)):app(element(2,G),[{len_out,{tag3,[F]}}]), V4;
app({tag3,[F]}, [X,Y]) -> V2=(element(1,F)):app(element(2,F),[F]), V3=(element(1,V2)):app(element(2,V2),[X,Y]), V3;
app({tag2,[]}, [F]) -> {len_out,{tag1,[F]}};
app({tag1,[F]}, [[],N]) -> N;
app({tag1,[F]}, [[_|T],N]) -> V1=(element(1,F)):app(element(2,F),[T,N + 1]), V1.
len4(S) -> G={len_out,{tag2,[]}}, H={len_out,{tag4,[G]}}, V5=(element(1,H)):app(element(2,H),[H]), V6=(element(1,V5)):app(element(2,V5),[S,0]), V6.
