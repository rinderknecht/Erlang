-module(catalan).
-export([preorder/1,postorder/1]).

% The number of steps is 2n + h + 2 if n > 0, else 1:
%
%   1     by clause 1 or 2 of postorder/1
% + 1     by clause 1 of postorder_/2
% + n     
% + n + h by clause 2 of postorder_/2
%
postorder(empty) -> [];
postorder(    T) -> postorder_([],[T]).

postorder_(Nodes,[]) ->
  Nodes;
postorder_(Nodes,[{Root,Subtrees}|Forest]) ->
  postorder_([Root|Nodes],hw:rev_append(Subtrees,Forest)).

% The number of steps is 4n + 2h + 3 if n > 0, else 1:
%
%     1 by clause 1 or 2 of preorder/1
% + n+2 by clause 1 of preorder_/2
% + n
% + 2n + 2h by clause 2 of preorder_/2  (???)
%
preorder(empty) -> [];
preorder(    T) -> preorder_([],[T]).

preorder_(Nodes,[]) ->
  hw:rev(Nodes);
preorder_(Nodes,[{Root,Subtrees}|Forest]) ->
  preorder_([Root|Nodes],Subtrees++Forest).

% 2010 version (without ++) based on flatten/1
% Delay?
%
preorder1(empty) -> [];
preorder1(    T) -> pre1([T]).

pre1([]) -> [];
pre1([{Root,Children}|F]) -> [Root|pre1([Children|F])];
pre1([             []|F]) -> pre1(F);
pre1([          [T|P]|F]) -> pre1([T,P|F]).

% 2010 another version with ++
%         1 by clause 1 or 2 of preorder2/1
%    +  n   by clause 2 of pre2/1
%    + 2n-1 by clause 1 and 2 (++) of pre2/1
%    = 3n
% (Same delay as preorder1/1).
%
%                   O
%                /  |  \
%               O   O   O
%              / \     / \
%             O   O   O   O
%                 |  /|\
%                 O O O O
%
preorder2(empty) -> [];
preorder2(    T) -> pre2([T]).

pre2([]) -> [];
pre2([{Root,Children}|F]) -> [Root|pre2(Children++F)].

