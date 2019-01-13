% Binary Search Trees (BST)
% 13 June 2008

-module(bst).
-compile(export_all).  % I am lazy. Don't do this.

%=====================================================================
% Membership. `mem(E,T)' is `false' if item `E' is not present in the
% BST `T', otherwise `true'.
%

% In tail form.
%
% In the worst case, the number of function calls to compute mem1(E,T)
% is n+1, where n is the number of nodes in `T'. This case occurs when
% `E' is not in `T' and `T' is a list.
%
% The number of comparisons is maximum when the tree is a
% right-leaning list. There are 2 failing comparisons for each node
% (clauses 2 and 3), so the worst case (i.e., the sought item is not
% present) requires exactly 2n comparisons.
%
mem1(_,            empty)               -> false;
mem1(E,{   _,   E,    _})               -> true;
mem1(E,{Left,Root,    _}) when E < Root -> mem1(E,Left);
mem1(E,{   _,   _,Right})               -> mem1(E,Right).

% In tail form
%
% This version performs n + 1 comparisons in the worst case.
%
mem(_,            empty) -> false;
mem(E,{Left,Root,Right}) -> mem__(E,{Left,Root,Right},Root).

mem__(E,{Left,Root,    _},C) when E < Root -> mem__(E,Left,C);
mem__(E,{   _,Root,Right},_)               -> mem__(E,Right,Root);
mem__(E,            empty,C)               -> E =:= C.

%=====================================================================
% `find(E,T,F)' finds item `E' in the BST `T' and `E' and the found
% subtree of `T' (whose root is `E') are then passed to the function
% `F', which returns a BST in place of the found subtree. The result
% of `find(E,T,F)' is therefore a tree identical to `T', except
% (perhaps) that the first subtree (in a preorder traversal) whose
% root is `E', say `S', is replaced by the tree resulting from the
% call to `F(E,S)'. If `F(E,S)=S' then `find(E,T,F) = T'.

% Not in tail form
%
find1(_,            empty,_) -> empty;
find1(E,{Left,   E,Right},F) -> F(E,{Left,E,Right});
find1(E,{Left,Root,Right},F) when E < Root -> 
  {find1(E,Left,F),Root,Right};
find1(E,{Left,Root,Right},F) ->
  {Left,Root,find1(E,Right,F)}.

% Almost in tail form
%
find(E,T,F) -> find(E,T,F,[]).

find(_,            empty,_,A) -> find_up(A,empty);
find(E,{Left,   E,Right},F,A) -> find_up(A,F(E,{Left,E,Right}));
find(E,{Left,Root,Right},F,A) when E < Root -> 
  find(E,Left,F,[{left,Root,Right}|A]);
find(E,{Left,Root,Right},F,A) ->
  find(E,Right,F,[{Left,Root,right}|A]).

find_up(                   [],T) -> T;
find_up([{left,Root,Right}|A],T) -> find_up(A,{T,Root,Right});
find_up([{Left,Root,right}|A],T) -> find_up(A,{Left,Root,T}).

%=====================================================================
% Adding an item as a leaf (without repetition)

% Not in tail form.
%
add_l1(E,            empty)               -> {empty,E,empty};
add_l1(E,{Left,   E,Right})               -> {Left,E,Right};
add_l1(E,{Left,Root,Right}) when E < Root -> {add_l1(E,Left),Root,Right};
add_l1(E,{Left,Root,Right})               -> {Left,Root,add_l1(E,Right)}.

% In tail form with sharing preserved if the item is already present.

add_tf(E,T)                                   -> add_tf(E,T,[],T).
add_tf(E,            empty,A,_)               -> appk(A,{empty,E,empty});
add_tf(E,       {_,   E,_},_,T)               -> T;
add_tf(E,{Left,Root,Right},A,T) when E < Root ->
  add_tf(E,Left,[{k1,Root,Right}|A],T);
add_tf(E,{Left,Root,Right},A,T)               ->
  add_tf(E,Right,[{k2,Left,Root}|A],T).
appk(                 [],V)                   -> V;
appk([{k1,Root,Right}|A],V)                   -> appk(A,{V,Root,Right});
appk( [{k2,Left,Root}|A],V)                   -> appk(A,{Left,Root,V}).

% Using a functional
%
add_l11(E,T) ->
  find(E,T,fun(I,empty) -> {empty,I,empty};
              (_,  Sub) -> Sub end).

% In Continuation-Passing Style, sharing preserved if item already present.
%
add2(E,Tree) -> add2(E,Tree,fun (X) -> X end,Tree).

add2(E,empty,K,_) ->
  K({empty,E,empty});
add2(E,{_,E,_},_,T) ->
  T;
add2(E,{Left,Root,Right},K,T) when E < Root ->
  add2(E,Left,fun (V) -> K({V,Root,Right}) end,T);
add2(E,{Left,Root,Right},K,T) ->
  add2(E,Right,fun (V) -> K({Left,Root,V}) end,T).

% In tail form (first-order)
%
add_l(E,Tree) -> add_l(E,Tree,[]).

add_l(E,empty,A) ->
  add_up(A,{empty,E,empty});
add_l(E,{Left,E,Right},A) ->
  add_up(A,{Left,E,Right});
add_l(E,{Left,Root,Right},A) when E < Root ->
  add_l(E,Left,[{left,Root,Right}|A]);
add_l(E,{Left,Root,Right},A) ->
  add_l(E,Right,[{Left,Root,right}|A]).

add_up(                   [],T) -> T;
add_up([{left,Root,Right}|A],T) -> add_up(A,{T,Root,Right});
add_up([{Left,Root,right}|A],T) -> add_up(A,{Left,Root,T}).

% Not in tail form
%
% This variation on add_l1/2 performs only n + 1 comparisons in the
% worst case.
%
add_l2(E,            empty) -> {empty,E,empty};
add_l2(E,{Left,Root,Right}) -> add_l2__(E,{Left,Root,Right},Root).

add_l2__(E,empty,C) ->
  if E =:= C -> empty;
     true    -> {empty,E,empty}
  end;
add_l2__(E,{Left,Root,Right},C) when E < Root ->
  {add_l2__(E,Left,C),Root,Right};
add_l2__(E,{Left,Root,Right},_) ->
  {Left,Root,add_l2__(E,Right,Root)}.

% In tail form.
%
% This variation on add_l/2 performs only n + 1 comparisons in the
% worst case.
%
add_l3(E,            empty) -> {empty,E,empty};
add_l3(E,{Left,Root,Right}) -> add_l3__(E,{Left,Root,Right},[],Root).

add_l3__(E,empty,A,C) ->
  if E =:= C -> add_up(A,empty);
     true    -> add_up(A,{empty,E,empty})
  end;
add_l3__(E,{Left,Root,Right},A,C) when E < Root ->
  add_l3__(E,Left,[{left,Root,Right}|A],C);
add_l3__(E,{Left,Root,Right},A,_) ->
  add_l3__(E,Right,[{Left,Root,right}|A],Root).

% Almost in tail form.
%
% This variation on add_l/2 avoids reconstructing the branch when E is
% already in T. This optimisation relies solely on the tail form.
%
add_l4(E,Tree) ->
  case add_l4(E,Tree,[]) of
    id -> Tree;
    T  -> T
  end.

add_l4(E,empty,A) ->
  add_up(A,{empty,E,empty});
add_l4(E,{_,E,_},_) ->
  id;
add_l4(E,{Left,Root,Right},A) when E < Root ->
  add_l4(E,Left,[{left,Root,Right}|A]);
add_l4(E,{Left,Root,Right},A) ->
  add_l4(E,Right,[{Left,Root,right}|A]).

% Almost in tail form.
%
% This variation on add_l3/2 performs only n + 1 comparisons in the
% worst case and does not rebuild the traversed branch if E was
% already in T.
%
add_l5(E,empty) -> {empty,E,empty};
add_l5(E,Tree={_,Root,_}) ->
  case add_l5__(E,Tree,[],Root) of
    id -> Tree;
    T  -> T
  end.

add_l5__(E,empty,A,C) ->
  if E =:= C -> id;
     true    -> add_up(A,{empty,E,empty})
  end;
add_l5__(E,{Left,Root,Right},A,C) when E < Root ->
  add_l5__(E,Left,[{left,Root,Right}|A],C);
add_l5__(E,{Left,Root,Right},A,_) ->
  add_l5__(E,Right,[{Left,Root,right}|A],Root).

% In tail form (higher-order)
%
add_l6(E,empty) -> {empty,E,empty};
add_l6(E,Tree={_,Root,_}) ->
  add_l6__(E,Tree,Root,Tree,fun(T)->T end).

add_l6__(E,empty,C,Orig,K) ->
  if E =:= C -> Orig;
     true    -> K({empty,E,empty})
  end;
add_l6__(E,{Left,Root,Right},C,Orig,K) when E < Root ->
  add_l6__(E,Left,C,Orig,fun (Tree) -> K({Tree,Root,Right}) end);
add_l6__(E,{Left,Root,Right},_,Orig,K) ->
  add_l6__(E,Right,Root,Orig,fun (Tree) -> K({Left,Root,Tree}) end).

%=====================================================================
% Adding an item as a root (without repetition)

% Note:
%
% {Left,Right} = split(E,T),{Left,E,Right}
%
% is exactly equivalent to
%
% (fun({Left,Right}) -> {Left,E,Right} end)(split(E,T))
%

% Not in tail form
%
add_r(E,T) -> {Left,Right} = split(E,T),{Left,E,Right}.

split(_,empty) ->
  {empty,empty};
split(E,{Left,E,Right}) ->
  {Left,Right};
split(E,{Left,Root,Right}) when E < Root ->
  {L,R} = split(E,Left),{L,{R,Root,Right}};
split(E,{Left,Root,Right}) ->
  {L,R} = split(E,Right),{{Left,Root,L},R}.

% In tail form
%
add_r2(E,T) -> split2([],[],E,T).

split2(Lt,Gt,E,empty) ->
  graft(Lt,E,Gt);
split2(Lt,Gt,E,{Left,E,Right}) ->
  graft([Left|Lt],E,[Right|Gt]);
split2(Lt,Gt,E,{Left,Root,Right}) when E < Root ->
  split2(Lt,[{Root,Right}|Gt],E,Left);
split2(Lt,Gt,E,{Left,Root,Right}) ->
  split2([{Left,Root}|Lt],Gt,E,Right).

graft(Lt,E,Gt) -> graft(empty,Lt,E,Gt,empty).

graft(T1,[],E,[],T2) ->
  {T1,E,T2};
graft(T1,[],E,[{Root,Right}|Gt],T2) ->
  graft(T1,[],E,Gt,{T2,Root,Right});
graft(T1,[{Left,Root}|Lt],E,[],T2) ->
  graft({Left,Root,T1},Lt,E,[],T2);
graft(T1,[{Left,Root1}|Lt],E,[{Root2,Right}|Gt],T2) ->
  graft({Left,Root1,T1},Lt,E,Gt,{T2,Root2,Right}).

%=====================================================================
% Removing an element (and grafting at a leaf)

% Not in tail form
%
rem_l1(_,             empty) -> empty;
rem_l1(E,{empty,   E,empty}) -> empty;
rem_l1(E,{empty,   E,Right}) -> Right;
rem_l1(E,{ Left,   E,empty}) -> Left;
rem_l1(E,{ Left,   E,Right}) -> hang1(Left,Right);
rem_l1(E,{ Left,Root,Right}) when E < Root ->
  {rem_l1(E,Left),Root,Right};
rem_l1(E,{Left,Root,Right}) ->
  {Left,Root,rem_l1(E,Right)}.

hang1(T,            empty) -> T; % T is hung at the leftmost leaf
hang1(T,{Left,Root,Right}) -> {hang1(T,Left),Root,Right}.

% Using a functional (in tail form because of hang/3).
%
hang(T,            empty,Forest) -> hang_up(T,Forest);
hang(T,{Left,Root,Right},Forest) -> hang(T,Left,[{Root,Right}|Forest]).

hang_up(Tree,[]) -> Tree;
hang_up(Left,[{Root,Right}|Forest]) ->
  hang_up({Left,Root,Right},Forest).

rem_l2(E,T) ->
  find(E,T,fun(_,empty)          -> empty;
              (_,{Left,_,Right}) -> hang(Left,Right,[]) end).

%=====================================================================
% Removing an element (and grafting a leaf instead)

% Not in tail form
%
rem_r1(_,             empty) -> empty;
rem_r1(E,{empty,   E,empty}) -> empty;
rem_r1(E,{empty,   E,Right}) -> Right;
rem_r1(E,{ Left,   E,empty}) -> Left;
rem_r1(E,{ Left,   E,Right}) -> {rem_r1(E,Left),max(Left),Right};%????
rem_r1(E,{ Left,Root,Right}) when E < Root ->
  {rem_r1(E,Left),Root,Right};
rem_r1(E,{Left,Root,Right}) ->
  {Left,Root,rem_r1(E,Right)}.

max({_,Root,empty}) -> Root;
max({_,   _,Right}) -> max(Right).

% Not in tail form
%
% This version decreases the number of function calls with respect to
% rem_r1/2 by making max2/1 return its argument without the maximum
% node, as well as the maximum node itself, as max/1 only does. This
% means that a recursive call to rem_r1/2 was necessary to remove
% Max. Here max2/1 interleaves both computations and results in a
% single visit to the nodes.
%
rem_r2(_,             empty) -> empty;
rem_r2(E,{empty,   E,empty}) -> empty;
rem_r2(E,{empty,   E,Right}) -> Right;
rem_r2(E,{ Left,   E,empty}) -> Left;
rem_r2(E,{ Left,   E,Right}) -> {L,Max} = max2(Left), {L,Max,Right};
rem_r2(E,{ Left,Root,Right}) when E < Root ->
  {rem_r2(E,Left),Root,Right};
rem_r2(E,{Left,Root,Right}) ->
  {Left,Root,rem_r2(E,Right)}.

max2({Left,Root,empty}) -> {Left,Root};
max2({Left,Root,Right}) -> {R,Max} = max2(Right), {{Left,Root,R},Max}.

% Using a functional (not in tail form)
%
rem_r3_x(_,         empty) -> empty;
rem_r3_x(_,{Left,_,Right}) -> {L,Max} = max2(Left), {L,Max,Right}.
    
rem_r3(E,T) -> find(E,T,fun rem_r3_x/2).

%=====================================================================
% Making a BST from a list and an addition function
%
% In tail form
%
make(L,Add) -> hw:foldl(Add,empty,L).
