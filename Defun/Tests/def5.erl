-module(def5).
-compile(export_all).

i(X) -> (fun tms/1)(X).
j(X) -> (fun(Y) -> tms(Y) end)(X).

% j(X) -> 
%   V2=(element(1,{'def5.out.erl',{tag2,[]}})):app(element(2,{'def5.out.erl',{tag2,[]}}),[X]),
%   V2.
% j(X) -> V2='def5.out.erl':app({tag2,[]},[X]), V2.
% j(X) -> V2=app({tag2,[]},[X]), V2.
% j(X) -> app({tag2,[]},[X])
