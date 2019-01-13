-module(exercises).
-compile(export_all).

%=====================================================================
%
-type(len,List[T] => Int).

len(   []) -> 0;
len([_|S]) -> 1 + len(S).

% Tail form
%
len0(S) -> len(S,0).

len(   [],N) -> N;
len([_|S],N) -> len(S,N+1).

%=====================================================================
%
sum(  [N]) -> N;
sum([N|S]) -> N + sum(S).

%=====================================================================
%
mem(_,   []) -> false;
mem(X,[X|_]) -> true;
mem(X,[_|S]) -> mem(X,S).

%=====================================================================
%
last(  [X]) -> X;
last([_|S]) -> last(S).

%=====================================================================
%
pen([I,_]) -> I;
pen([_|S]) -> pen(S).

%=====================================================================
% `occ(X,S)' is the number of occurrences of X in the list S.
%
occ(_,   []) -> 0;
occ(X,[X|S]) -> 1 + occ(X,S);
occ(X,[_|P]) -> occ(X,P).
    
%=====================================================================
% `index(X,S,N)' is the index of the first occurrence of X in list S,
% the first item having index N (integer), or the atom `absent' if 
% not found.
%
index(_,   [],_) -> absent;
index(X,[X|_],N) -> N;
index(X,[_|S],N) -> index(X,S,N+1).

%=====================================================================
% `indices(X,S,N)' is the list of the indices of all occurrences of X
% in list S, in increasing order, the first item having index N
% (integer).
%
indices(_,   [],_) -> [];
indices(X,[X|S],N) -> [N|indices(X,S,N+1)];
indices(X,[_|S],N) -> indices(X,S,N+1).

%=====================================================================
% `rcat(P,Q)' is the list whose first items are the items of `P'
% reversed, followed by the items of `Q' in the same order.
%
rcat(   [],Q) -> Q;
rcat([I|P],Q) -> rcat(P,[I|Q]).

%=====================================================================
% `cat(P,Q)' is the list whose first items are those of `P' in the
% same order, and the following are those of `Q' in the same order. 
% In other words, `cat(P,Q)' is the catenation of P and Q.
%
cat(   [],T) -> T;
cat([X|S],T) -> [X|cat(S,T)].

% Tail form
%
cat0(S,T) -> cat0(S,T,[]).

cat0(   [],T,[X|U]) -> cat0([],[X|T],U);
cat0(   [],T,   []) -> T;
cat0([X|S],T,    U) -> cat0(S,T,[X|U]).

% Variant
%
cat1(S,T) -> cat1(S,T,[]).

cat1(   [],T,U) -> rcat(U,T);
cat1([X|S],T,U) -> cat1(S,T,[X|U]).

% %=====================================================================
% `rev(L)' is the list containing all the items of `L' reversed.
%
rev(L) -> rcat(L,[]).

% Slow variant
%
rev0(   []) -> [];
rev0([I|L]) -> cat(rev0(L),[I]).

%====================================================================
% Shuffling two lists
%
shuffle(   [],T) -> T;
shuffle([X|S],T) -> [X|shuffle(T,S)].

% Variant
%
shuffle1(   [],    T) -> T;
shuffle1(    S,   []) -> S;
shuffle1([X|S],[Y|T]) -> [X,Y|shuffle1(S,T)].

% Perfect shuffle (not tail form)
%
shuffle2(   [],   []) -> [];
shuffle2([X|S],[Y|T]) -> [X,Y|shuffle2(S,T)].

%=====================================================================
%
sfst(_,   []) -> [];
sfst(X,[X|S]) -> S;
sfst(X,[Y|S]) -> [Y|sfst(X,S)].

% Tail form
%
sfst0(X,S) -> sfst(X,S,[],S).

sfst(_,   [],_,U) -> U;
sfst(X,[X|S],T,_) -> rcat(T,S);
sfst(X,[Y|S],T,U) -> sfst(X,S,[Y|T],U).     

% Bad design
%
sfst1(X,S) -> sfst2(X,S,[],S).

sfst2(_,   [],_,U) -> U;
sfst2(X,[X|S],T,_) -> cat(T,S);
sfst2(X,[Y|S],T,U) -> sfst2(X,S,cat(T,[Y]),U).    

%=====================================================================
%
sall(   [],_) -> [];
sall(X,[X|S]) -> sall(X,S);
sall(X,[Y|S]) -> [Y|sall(X,S)].

% Tail form
%
sall0(X,S) -> sall0(X,S,[]).

sall0(_,   [],T) -> rev(T);
sall0(X,[X|S],T) -> sall0(X,S,T);
sall0(X,[Y|S],T) -> sall0(X,S,[Y|T]).

%=====================================================================
%
slst0(X,S) -> rev(sfst(X,rev(S))).

% Variant
%
slst(_,   []) -> [];
slst(X,[X|S]) -> slst(X,S,S);
slst(X,[Y|S]) -> [Y|slst(X,S)].    

slst(_,   [],T) -> T;
slst(X,[X|_],T) -> [X|slst(X,T)];
slst(X,[Y|S],T) -> slst(X,S,T).

%=====================================================================
%
flat0(         []) -> [];
flat0(     [[]|T]) -> flat0(T);
flat0([S=[_|_]|T]) -> cat(flat0(S),flat0(T));
flat0(      [Y|T]) -> [Y|flat0(T)].

% Variant
%
flat1(       []) -> [];
flat1(   [[]|T]) -> flat1(T);
flat1([[X|S]|T]) -> flat1([X,S|T]);
flat1(    [Y|T]) -> [Y|flat1(T)].

% Variant
%
flat2(       []) -> [];
flat2(   [[]|T]) -> flat2(T);
flat2(  [[X]|T]) -> flat2([X|T]);
flat2([[X|S]|T]) -> flat2([X,S|T]);
flat2(    [Y|T]) -> [Y|flat2(T)].

% Tail form of flat2.
%
flat3(T) -> flat3(T,[]).

flat3(       [],U) -> rev(U);
flat3(   [[]|T],U) -> flat3(T,U);
flat3(  [[X]|T],U) -> flat3([X|T],U);
flat3([[X|S]|T],U) -> flat3([X,S|T],U);
flat3(    [Y|T],U) -> flat3(T,[Y|U]).

% Variant of flat3/1
%
% flat4(T) -> flat4(T,[]).

% flat4(         [],U) -> rev(U);
% flat4(     [[]|T],U) -> flat4(T,U);
% flat3(    [[X]|T],U) -> flat3([X|T],U);
% flat4([S=[_|_]|T],U) -> flat4(Tflat4(,T),L);
% flat4(      [Y|T],U) -> flat4(T,[Y|U]).

    
% This version is in tail form and uses simpler patterns.
% Credit: Jeong Jeong Hi
%
flat4(L) -> flat4(L,[],[]).

flat4(   [],[],Q) -> rev(Q);
flat4(   [], P,Q) -> flat4(P,   [],    Q);
flat4(  [I], P,Q) -> flat4(I,    P,    Q);
flat4([I|L], P,Q) -> flat4(I,[L|P],    Q);
flat4(    I, P,Q) -> flat4(P,   [],[I|Q]).

% In Continuation-Passing Style
%
% flat_k(L)           -> flat_k(L,fun(V) -> V end).
% flat_k(       [],K) -> K([]);
% flat_k(   [[]|Q],K) -> flat_k(Q,K);
% flat_k([[I|P]|Q],K) -> 
%   flat_k([I|P],fun(V) -> flat_k(Q, fun(W) -> cat_k(V,W,K) end) end);
% flat_k(    [I|Q],K) -> flat_k(Q,fun(V) -> K([I|V]) end).
% cat_k(   [],Q,K)      -> K(Q);
% cat_k([I|P],Q,K)      -> cat_k(P,Q,fun(V) -> K([I|V]) end).

% Variant of flat4/1
%
flat5(L) -> flat5(L,[]).

flat5(   [],[]) -> [];
flat5(   [], P) -> flat5(P,[]);
flat5(  [I], P) -> flat5(I,P);
flat5([I|L], P) -> flat5(I,[L|P]);
flat5(    I, P) -> [I|flat5(P,[])].

% Variant
%
flat6(T) -> flat6(T,[]).

flat6([],S) -> S;
flat6([T1|T2],S) -> flat6(T1,flat6(T2,S));
flat6(X,S) -> [X|S].    

% With a counter
%
flat6c(T) -> flat6c(T,[],1).

flat6c([],S,C) -> {S,C+1};
flat6c([T1|T2],S,C) -> {V,D}=flat6c(T2,S,C+1),flat6c(T1,V,D);
flat6c(X,S,C) -> {[X|S],C+1}.

%=====================================================================
% Packing
%

% Erroneous variant
%
pack_err(          []) -> [];
pack_err([[I|Is],I|L]) -> pack_err([[I,I|Is]|L]);
pack_err(  [[I|Is]|L]) -> [[I|Is]|pack_err(L)];
pack_err(       [I|L]) -> pack_err([[I]|L]).

% Same as before but combinations are sorted lexicographically in
% increasing order.
%
combine1(_,0)            -> [];
combine1(L,N) when N > 0 -> rev(combine1([],L,N)).
 
combine1(Comb,[],_) ->
    Comb;
combine1([],L,1) ->
    foldl(fun (I,A) -> [[I]|A] end,[],L);
combine1(Comb,[I|L],N) ->
    SubComb = combine1([],L,N-1),
    NewComb = foldr(fun (M,A) -> [[I|M]|A] end, Comb, SubComb),
    combine1(NewComb,L,N).

%=====================================================================
% foldl(F,A,[I1,I2,...,In]) = F(In,F(...F(I2,F(I1,A))...))
% foldl(F,A,[]) = A
%
% Tail form.
%
% foldl(_,A,   []) -> A;
% foldl(F,A,[I|L]) -> foldl(F,F(I,A),L).

%=====================================================================
% foldr(F,A,[I1,I2,...,In]) = F(I1,F(I2,...,F(In,A))...)
% foldr(F,A,[]) = A
%
% Not in tail form.
%
% foldr(_,A,   []) -> A;
% foldr(F,A,[I|L]) -> F(I,foldr(F,A,L)).

% Filtering out the unique items
%

% Order preserved
%
uniq0(   []) -> [];
uniq0([I|L]) -> case rma(I,L) of
                  {_,absent} -> [I|uniq0(L)];
                  {P, found} -> uniq0(P)
                end.

rma(_,   []) -> {[],absent};
rma(I,[I|L]) -> {P,_}=rma(I,L), {P,found};
rma(I,[J|L]) -> {P,S}=rma(I,L), {[J|P],S}.
    
% Order not preserved
%
uniq1(   []) -> [];
uniq1([I|L]) -> rmax(I,L,[],absent).

rmax(_,   [],Q, found) -> uniq1(Q);
rmax(I,   [],Q,absent) -> [I|uniq1(Q)];
rmax(I,[I|P],Q,     _) -> rmax(I,P,Q,found);
rmax(I,[J|P],Q,     S) -> rmax(I,P,[J|Q],S).
