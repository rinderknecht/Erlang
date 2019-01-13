%{
(* Parser for the standalone DSL. This specification requires the
   parser generator Menhir. *)

let extract (error: string) (id: string) (a: int) =
  let rec check = function
    [] -> []
  | (name,seg,((pat,_,_) as clause))::clauses ->
      if name <> id 
      then Error.signal error seg
      else if List.length pat <> a
           then Error.signal "Arity mismatch in pattern" seg
           else clause::check clauses
in check

exception Workflow of Error.seg

let rec chk_erl : Ast.expr -> unit = 
  let open Ast 
in function
  LetIn (_,_,_,_) | OrElse (_,_,_) | AndAlso (_,_,_) | Leq (_,_,_)
| Lt (_,_,_) | Geq (_,_,_) | Gt (_,_,_) | Neq (_,_,_)
| Eq (_,_,_) | App (_,_,_) | Plus (_,_,_) | Minus (_,_,_)
| Ratio (_,_,_) | Mult (_,_,_) | Div (_,_,_) | Rem (_,_,_)
| Case (_,_,_) | Try (_,_,_,_) | Throw (_,_)
| Ref (_,_,_) | Lambda (_,_) | Call (_,_,_) | Eatom (_,_)
    -> ()
| Iter (_,_,s) | Do (_,_,_,s) | Ret (_,s) | Ctrl (_,_,_,s)
| Par (_,_,s) | Any (_,_,s) | Dist (_,_,_,s) | Exec (_,s) | Cont (_,s)
| Delay (_,s) | Retry (_,_,s) | Raise (_,s) | Tref (_,_,s)
| Tlam (_,s) | Bind (_,_,s) | Drop (_,_,s) | Insp (_,_,s)
    -> raise (Workflow s)
| Not (e,_) | Uplus (e,_) | Uminus (e,_) -> chk_erl e
| If (ilist,_) ->
   let app (e1,e2,_) = chk_erl e1; chk_erl e2
   in List.iter app ilist
| Elist (h,None) -> List.iter chk_erl h
| Elist (h,Some t) -> List.iter chk_erl h; chk_erl t
| Etuple comp -> List.iter chk_erl comp

let rec mk_patt : Ast.expr -> Ast.pattern = 
  let open Ast
in function
  LetIn(_,_,_,s) -> Error.signal "No let-in allowed in pattern." s
| OrElse(_,_,s) | AndAlso(_,_,s)  | Not(_,s) ->
    Error.signal "No boolean operator allowed in pattern." s
| Leq(_,_,s) | Lt(_,_,s) | Geq(_,_,s) | Gt(_,_,s)
| Neq(_,_,s) | Eq(_,_,s) ->
    Error.signal "No comparison allowed in pattern." s
| App(_,_,s) ->
    Error.signal "No list appending allowed in pattern." s
| Plus(_,_,s) | Minus(_,_,s) | Ratio(_,_,s)
| Mult(_,_,s) | Div(_,_,s) | Rem(_,_,s) ->
    Error.signal "No arithmetic operator allowed in pattern." s
| Uplus(e,s) ->
    (match mk_patt e with
       Patom(_,_) as a -> a
     | _ -> Error.signal "No unary plus allowed in pattern." s)
| Uminus(e,s) ->
    (match mk_patt e with
        Patom(_,_) as a -> a
      | _ -> Error.signal "Incorrect unary minus in pattern." s)
| If(_,s) -> Error.signal "No conditional allowed in pattern." s
| Case(_,_,s) ->
    Error.signal "No case allowed in pattern." s
| Try(_,_,_,s) ->
    Error.signal "No try allowed in pattern." s
| Throw(_,s) ->
    Error.signal "No throw allowed in pattern." s
| Lambda(_,s) ->
    Error.signal "No lambda allowed in pattern." s
| Ref(_,_,s) ->
    Error.signal "No function name allowed in pattern." s
| Call(_,_,s) ->
    Error.signal "No call allowed in pattern." s
| Eatom(a,s) -> Patom(a,s)
| Elist(h,None)   -> Plist(List.map mk_patt h, None)
| Elist(h,Some t) -> Plist(List.map mk_patt h, Some (mk_patt t))
| Etuple t   -> Ptuple(List.map mk_patt t)
| Iter (_,_,s) | Do (_,_,_,s) | Ret (_,s) | Ctrl (_,_,_,s)
| Par (_,_,s) | Any (_,_,s) | Dist (_,_,_,s) | Exec (_,s) | Cont (_,s)
| Delay (_,s) | Retry (_,_,s) | Raise (_,s) | Tref (_,_,s)
| Tlam (_,s) | Bind (_,_,s) | Drop (_,_,s) | Insp (_,_,s) ->
    Error.signal "No task allowed in pattern." s

%}

/* The pure sub-language */

/* Keywords */

%token AFTER ANDALSO CASE CATCH DIV END FALSE 
%token FUN IF NOT OF ORELSE REM THROW TRUE TRY WHEN
%token MODULE COMPILE EXPORT_ALL TYPE
%token ANY_TYPE FLOAT INTEGER CHAR STRING BOOLEAN LIST

/* Symbols */

%token COMMA     /* ,   */
%token SEMI      /* ;   */
%token DOT       /* .   */

%token LPAR      /* (   */
%token RPAR      /* )   */
%token LBRACKET  /* [   */
%token RBRACKET  /* ]   */
%token LBRACE    /* {   */
%token RBRACE    /* }   */

%token TIMES     /* *   */
%token SLASH     /* /   */
%token PLUS      /* +   */
%token MINUS     /* -   */

%token MID       /* |   */
%token ARROW     /* ->  */
%token DEF       /* =   */
%token APPEND    /* ++  */

%token EQ        /* =:= */
%token NEQ       /* =/= */
%token LT        /* <   */
%token LEQ       /* =<  */
%token GT        /* >   */
%token GEQ       /* >=  */
%token Wild      /* _   */

%token Tdef      /* :: */

/* Literals */

%token <string> Var
%token <string> String
%token <string> Atom
%token <int> Nat
%token <string> Float
%token <string * string> QName

/* The workflow sub-language (tasks) */

/* Keywords */

%token ACCUMULATOR ANY BY CONTINUE CONTROLLED
%token DELAY DO DISTRIBUTE EVERY ERROR EXECUTE FOR
%token INSPECT ITERATE NODE NODES
%token ON ONE OR PROVIDING RAISE RETRY RETURN SELECTED 
%token STABLE TASK WITH

/* Symbols */

%token BIND /* >> */
%token DROP /* >>| */
%token PAR  /* || */

/* End-of-file Sentinel */

%token EOF

/* Entries */

%start main
%type <Ast.t> main

%%

/* Grammar */

main:
  m=module_name compile_opt t=type_spec* d=fun_or_task* EOF {
  m, t, d }

type_spec:
  TYPE p=poly_type Tdef e=type_expr DOT { p, e }

poly_type:
  t=Atom LPAR args=type_vars RPAR { t, args }

type_vars:
  /* empty */                {      [] }
| v=Var COMMA more=type_vars { v::more }

type__:
  ANY_TYPE LPAR RPAR            { Ast.Any         }
| FLOAT LPAR RPAR               { Ast.Float       }
| INTEGER LPAR RPAR             { Ast.Int         }
| CHAR LPAR RPAR                { Ast.Char        }
| STRING LPAR RPAR              { Ast.Str         }
| BOOLEAN LPAR RPAR             { Ast.Bool        }
| FUN LPAR f=fun_type RPAR      { Ast.Fun f       }
| LIST LPAR t=type__ RPAR       { Ast.List t      }
| LBRACE types=type_list RBRACE { Ast.Tuple types }
| p=poly_type                   { Ast.Poly p      }

fun_type:
  LPAR a=type_list RPAR ARROW r=type__ { a, r }

type_list:
  /* empty */   { [] }
| l=type_nlist  {  l }

type_nlist:
  t=type__                       {     [t] }
| t=type__ COMMA more=type_nlist { t::more }

type_expr:
  t=type__         { Ast.Texpr t }
| s=val_const_list { Ast.Tsum s  }

val_const_list:
  /* empty */                         {      [] }
| c=val_const MID more=val_const_list { c::more }

val_const:
  c=Atom                                  { c, [] }
| LBRACE c=Atom COMMA t=type_nlist RBRACE { c,  t }

module_name: MODULE LPAR a=Atom RPAR DOT { a }

compile_opt: COMPILE LPAR EXPORT_ALL RPAR DOT { }

fun_or_task:
  d=fun_def  { Ast.Func (d,($startpos(d),$endpos(d))) }
| d=task_def { Ast.Task (d,($startpos(d),$endpos(d))) }

fun_def:
  d=semi_list(named_fun_clause) DOT {
    match d with
                 [] -> assert false
    | (n,_,((p,_,_) as c))::more ->
      let e = "Inconsistent function name." in
        let a = List.length p
      in n, a, c::extract e n a more
  }

semi_list(Item):
  i=Item                           {     [i] }
| i=Item SEMI more=semi_list(Item) { i::more }

named_fun_clause:
  name=Atom clause=fun_clause {
    let seg = $startpos(name), $endpos(name)
    in name, seg, clause
  }

fun_clause:
  LPAR p=patterns RPAR g=option(guard) ARROW e=expr { p,g,e }

patterns:
  /* empty */                   {      [] }
| p=pattern                     {     [p] }
| p=pattern COMMA more=patterns { p::more }

pattern:
  a=atomic                        { Ast.Patom a  }
| l=dsl_list(pattern)             { Ast.Plist l  }
| t=tuple(patterns)               { Ast.Ptuple t }
| p1=pattern d=DEF p2=pattern_000 {
    Ast.Palias(p1,p2,($startpos(d),$endpos(d)))  }

pattern_000:
  a=atomic            { Ast.Patom a  }
| l=dsl_list(pattern) { Ast.Plist l  }
| t=tuple(patterns)   { Ast.Ptuple t }
  
atomic:
  v=Var    { Ast.Var v,    ($startpos(v),$endpos(v)) }
| n=Nat    { Ast.Nat n,    ($startpos(n),$endpos(n)) }
| s=Float  { Ast.Float s,  ($startpos(s),$endpos(s)) }
| a=Atom   { Ast.Atom a,   ($startpos(a),$endpos(a)) }
| s=String { Ast.String s, ($startpos(s),$endpos(s)) }
| t=TRUE   { Ast.True,     ($startpos(t),$endpos(t)) }
| f=FALSE  { Ast.False,    ($startpos(f),$endpos(f)) }
| w=Wild   { Ast.Wild,     ($startpos(w),$endpos(w)) }

dsl_list(Item):
  LBRACKET RBRACKET               { [], None }
| LBRACKET i=Item more=tail(Item) { let h,t = more in i::h,t }

dsl_nlist(Item):
  LBRACKET i=Item more=tail(Item) { let h,t = more in i::h,t }

tail(Item):
  RBRACKET                     { [], None }
| MID i=Item RBRACKET          { [], Some i }
| COMMA i=Item more=tail(Item) { let h,t = more in i::h,t }

tuple(Items):
  LBRACE l=Items RBRACE { l }

guard:
  WHEN e=expr_150 { e, ($startpos(e),$endpos(e)) }

exprs:
  e=expr                  {     [e] }
| e=expr COMMA more=exprs { e::more }

expr:
  e1=expr_750 d=DEF e2=expr COMMA e3=expr {
    let open Ast
    in LetIn(mk_patt e1, e2, e3, ($startpos(d),$endpos(d)))
  }
| e=expr_150 { e }

expr_150:
  e1=expr_160 o=ORELSE e2=expr_150 {
    Ast.OrElse(e1,e2,($startpos(o),$endpos(o))) }
| e=expr_160 { e }

expr_160:
  e1=expr_200 a=ANDALSO e2=expr_160 {
    Ast.AndAlso(e1,e2,($startpos(a),$endpos(a))) }
| e=expr_200 { e }

expr_200:
  e1=expr_300 c=comp_op e2=expr_300 { c e1 e2 }
| e=expr_300                        { e       }

expr_300:
  e1=expr_400 a=APPEND e2=expr_300 {
    Ast.App(e1,e2,($startpos(a),$endpos(a))) }
| e=expr_400 { e }

expr_400:
  e1=expr_400 op=add_op e2=expr_500 { op e1 e2 }
| e=expr_500                        {        e }

expr_500:
  e1=expr_500 op=mult_op e2=expr_600 { op e1 e2 }
| e=expr_600                         {        e }

expr_600:
  op=prefix_op e=expr_700 { op e }
| e=expr_700              {    e }

prefix_op:
  o=PLUS  { fun e -> Ast.Uplus(e,($startpos(o),$endpos(o)))  }
| o=MINUS { fun e -> Ast.Uminus(e,($startpos(o),$endpos(o))) }
| o=NOT   { fun e -> Ast.Not(e,($startpos(o),$endpos(o)))    }

expr_700:
  e=expr_750               { e }
| e=if_expr                { e }
| e=case_expr              { e }
| e=fref_and_lam           { e }
| e=try_expr               { e }
| t=THROW LPAR e=expr RPAR {
    Ast.Throw(e,($startpos(t),$endpos(t))) }

expr_750:
  LPAR e=expr RPAR         { e }
| LPAR e1=expr RPAR a=args {
    let s,e2 = a in Ast.Call(e1,e2,s)
  }

| f=Atom a=args {
    let open Ast in
      let s,e2 = a in
      let atom = Eatom (Atom f,($startpos(f),$endpos(f)))
    in Call(atom,e2,s)
  }
/*
| q=QName a=args { 
    let s,e2 = a in
      let open Ast in
      let qname = Eatom (QName q,($startpos(q),$endpos(q)))
    in Call (qname,e2,s)
  }
*/
| v=Var a=args {
    let open Ast in
      let s,e2 = a in
      let var = Eatom (Var v,($startpos(v),$endpos(v)))
    in Call(var,e2,s)
  }

| a=atomic         { Ast.Eatom a  }
| e=dsl_list(expr) { Ast.Elist e  }
| e=tuple(exprs)   { Ast.Etuple e }

args:
  o=LPAR a=option(exprs) RPAR {
    ($startpos(o),$endpos(o)),
     match a with None -> [] | Some e -> e }

if_expr:
  i=IF c=semi_list(if_clause) END {
    Ast.If(c,($startpos(i),$endpos(i))) }

if_clause:
  e1=expr_150 a=ARROW e2=expr {
    e1, e2, ($startpos(a),$endpos(a)) }

case_expr:
  c=CASE e=expr OF clauses=case_clauses END {
    Ast.Case(e,clauses,($startpos(c),$endpos(c))) }

case_clauses:
  l=semi_list(case_clause) { l }

case_clause:
  p=pattern g=option(guard) ARROW e=expr { p,g,e }

fref_and_lam:
  f=FUN id=Atom SLASH n=Nat {
    Ast.Ref((None,id),n,($startpos(f),$endpos(f))) }
/*
| f=FUN q=QName SLASH n=Nat {
    let m,id = q
    in Ast.Ref((Some m,id),n,($startpos(f),$endpos(f))) }
*/
| f=FUN clauses=semi_list(fun_clause) e=END {
    Ast.Lambda(clauses,($startpos(f),$endpos(e))) }

try_expr:
  t=TRY e=expr cases=try_cases exn=catch close=END {
    Ast.Try(e,cases,exn,($startpos(t),$endpos(close)))
  }
 
try_cases:
  OF clauses=try_clauses { clauses }
| /* empty */            {      [] }

try_clauses:
  clauses=case_clauses { clauses }

catch:
  CATCH clauses=catch_clauses { clauses }

catch_clauses:
  l=semi_list(catch_clause) { l }

catch_clause:
  p=pattern ARROW e=expr { p,e }

add_op:
  o=PLUS  { fun e1 e2 -> Ast.Plus(e1,e2,($startpos(o),$endpos(o)))  }
| o=MINUS { fun e1 e2 -> Ast.Minus(e1,e2,($startpos(o),$endpos(o))) }

mult_op:
  o=SLASH { fun e1 e2 -> Ast.Ratio(e1,e2,($startpos(o),$endpos(o))) }
| o=TIMES { fun e1 e2 -> Ast.Mult(e1,e2,($startpos(o),$endpos(o)))  }
| o=DIV   { fun e1 e2 -> Ast.Div(e1,e2,($startpos(o),$endpos(o)))   }
| o=REM   { fun e1 e2 -> Ast.Rem(e1,e2,($startpos(o),$endpos(o)))   }

comp_op:
  c=LEQ { fun e1 e2 -> Ast.Leq(e1,e2,($startpos(c),$endpos(c))) }
| c=LT  { fun e1 e2 -> Ast.Lt(e1,e2,($startpos(c),$endpos(c)))  }
| c=GEQ { fun e1 e2 -> Ast.Geq(e1,e2,($startpos(c),$endpos(c))) }
| c=GT  { fun e1 e2 -> Ast.Gt(e1,e2,($startpos(c),$endpos(c)))  }
| c=NEQ { fun e1 e2 -> Ast.Neq(e1,e2,($startpos(c),$endpos(c))) }
| c=EQ  { fun e1 e2 -> Ast.Eq(e1,e2,($startpos(c),$endpos(c)))  }


/*------------------------------------------------------------*/
/* Tasks */

task_def:
  TASK d=semi_list(named_task_clause) DOT { 
    match d with
      [] -> assert false
    | (n,_,((p,_,_) as c))::more ->
      let e = "Inconsistent task name." in
        let a = List.length p
      in n, a, c::extract e n a more
  }

named_task_clause:
  name=Atom clause=task_clause { 
    let seg = $startpos(name), $endpos(name)
    in name, seg, clause
  }

task_clause:
  LPAR p=patterns RPAR g=option(guard) ARROW e=task_expr { p,g,e }

task_exprs:
  e=task_expr                       {     [e] }
| e=task_expr COMMA more=task_exprs { e::more }

/* In [task_expr], if [e2] is a task, then [e1] is a variable,
   otherwise [e1] is a pattern. */

task_expr:
  e1=task_expr_750 d=DEF e2=task_expr_025 COMMA e3=task_expr {
   let open Ast in
     let def = $startpos(d),$endpos(d) in
     let p = mk_patt e1
   in try chk_erl e2; chk_erl e3; LetIn(p,e2,e3,def) with
        Workflow s -> (match p with
                         Patom(Var _, _) -> LetIn(p,e2,e3,def)
                       | _ -> Error.signal "Variable expected" s)
  }
| e=task_expr_025 { e }

task_expr_025:
  k=ITERATE e=task_expr_800 a=option(accumulator) {
    Ast.Iter(e,a,($startpos(k),$endpos(k)))
  }
| e=task_expr_050 { e }

task_expr_050:
  d=DO t1=task_expr_100 OR t2=task_expr_100 a=AFTER e=expr {
    Ast.Do(t1,t2,e,($startpos(d),$endpos(d)))
  }
| r=RETURN e=expr {
    Ast.Ret(e,($startpos(r),$endpos(r)))
  }
| e=task_expr_100 c=CONTROLLED b=BY t=tref_and_tlam 
  a=option(accumulator) {
    Ast.Ctrl(e,t,a,($startpos(c),$endpos(b)))
  }
| e=task_expr_100 { e }

task_expr_100:
  a=ANY e1=expr OF e2=task_expr_150 p=PAR e3=task_expr_125 {
    Ast.Any(e1,
            Ast.Par(e2,e3,($startpos(p),$endpos(p))),
            ($startpos(a),$endpos(a))) 
  }
| e=task_expr_125 { e }

task_expr_125:
  e1=task_expr_125 p=PAR e2=task_expr_150 {
    Ast.Par(e1,e2,($startpos(p),$endpos(p))) }
| e=task_expr_150 { e }

accumulator:
  WITH ACCUMULATOR e=expr { e }

task_expr_150:
  d=DISTRIBUTE e=task_expr_100 ON n=nodes co=option(constraints) {
    match co with
      None   -> let empty = ([],None),None
                in Ast.Dist(e,n,empty,($startpos(d),$endpos(d)))
    | Some c -> Ast.Dist(e,n,c,($startpos(d),$endpos(d)))
  }
| exec=EXECUTE e=task_expr_150 {
    Ast.Exec(e,($startpos(exec),$endpos(exec)))
  }
| c=CONTINUE w=WITH e=expr {
    Ast.Cont(e,($startpos(c),$endpos(w)))
  }
| d=DELAY e=expr { 
    Ast.Delay (e,($startpos(d),$endpos(d)))
  }
| r=RETRY e1=task_expr_150 EVERY e2=expr {
    Ast.Retry(e1,e2,($startpos(r),$endpos(r)))
  }
| e=task_expr_155 { e }

nodes:
  NODES    { Ast.Many  }
| ONE NODE { Ast.One }

/* Each expression below should evaluated into an atom. */
constraints:
  PROVIDING l=dsl_nlist(expr) s=option(selected) { l,s }

selected:
  SELECTED BY e=fref_and_lam { e }

task_expr_155:
  e1=task_expr_160 o=ORELSE e2=task_expr_155 {
    Ast.OrElse(e1,e2,($startpos(o),$endpos(o))) }
| e=task_expr_160 { e }

task_expr_160:
  e1=task_expr_200 a=ANDALSO e2=task_expr_160 {
    Ast.AndAlso(e1,e2,($startpos(a),$endpos(a)))
  }
| e=task_expr_200 { e }

task_expr_200:
  e1=task_expr_300 c=comp_op e2=task_expr_300 { c e1 e2 }
| e=task_expr_300                             { e       }

task_expr_300:
  e1=task_expr_400 a=APPEND e2=task_expr_300 {
    Ast.App(e1,e2,($startpos(a),$endpos(a))) }
| e=task_expr_400 { e }

task_expr_400:
  e1=task_expr_400 op=add_op e2=task_expr_500 { op e1 e2 }
| e=task_expr_500                             { e        }

task_expr_500:
  e1=task_expr_500 op=mult_op e2=task_expr_600 { op e1 e2 }
| e=task_expr_600                              { e        }

task_expr_600:
  op=prefix_op e=task_expr_700 { op e }
| e=task_expr_700              { e    }

task_expr_700:
  e=if_task_expr           { e }
| e=case_task_expr         { e }
| e=task_fref_and_lam      { e }
| e=try_task_expr          { e }
| e=tref_and_tlam          { e }
| e=task_expr_750          { e }
| e=inspect_expr           { e }
| r=RAISE LPAR e=expr RPAR {
    Ast.Raise (e,($startpos(r),$endpos(r))) }

if_task_expr:
  i=IF c=semi_list(if_task_clause) END {
    Ast.If(c,($startpos(i),$endpos(i))) }

if_task_clause:
  e1=expr_150 a=ARROW e2=task_expr {
    e1, e2, ($startpos(a),$endpos(a)) }

case_task_expr:
  c=CASE e=expr OF clauses=case_task_clauses END {
    Ast.Case(e,clauses,($startpos(c),$endpos(c))) }

case_task_clauses:
  l=semi_list(case_task_clause) { l }

case_task_clause:
  p=pattern g=option(guard) ARROW e=task_expr { p,g,e }

task_fref_and_lam:
  f=FUN id=Atom SLASH n=Nat {
    Ast.Ref((None,id),n,($startpos(f),$endpos(f))) }
/*
| f=FUN q=QName SLASH n=Nat {
    let m,id = q
    in Ast.Ref((Some m,id),n,($startpos(f),$endpos(f))) }
*/
| f=FUN clauses=semi_list(task_clause) e=END {
    Ast.Lambda(clauses,($startpos(f),$endpos(e))) }

try_task_expr:
  t=TRY e=expr cases=try_task_cases exn=task_catch close=END {
    Ast.Try(e,cases,exn,($startpos(t),$endpos(close)))
  }
 
try_task_cases:
  OF clauses=try_task_clauses { clauses }
| /* empty */                 {      [] }

try_task_clauses:
  clauses=case_task_clauses { clauses }

task_catch:
  CATCH clauses=catch_task_clauses { clauses }

catch_task_clauses:
  l=semi_list(catch_task_clause) { l }

catch_task_clause:
  p=pattern ARROW e=task_expr { p,e }

task_expr_750:
  e1=task_expr_750 b=BIND e2=task_expr_775 {
    Ast.Bind(e1,e2,($startpos(b),$endpos(b)))
  }
| e1=task_expr_750 d=DROP e2=task_expr_775 {
    Ast.Drop(e1,e2,($startpos(d),$endpos(d)))
  }
| e=task_expr_800 { e }

task_expr_775:
  e=tref_and_tlam  { e }
| LPAR e=expr RPAR { e }

task_expr_800:
  LPAR e=task_expr RPAR { e }
| LPAR e1=task_expr RPAR a=args {
    let open Ast
    in try chk_erl e1; let s,e2 = a in Call(e1,e2,s) with
         Workflow s ->
           Error.signal "Arguments cannot be applied to tasks" s
  }
| f=Atom a=args { 
    let s,e2 = a in
      let open Ast in
      let atom = Eatom (Atom f,($startpos(f),$endpos(f)))
    in Call (atom,e2,s)
 }
/*
| q=QName a=args {
    let s,e2 = a in
      let open Ast in
      let atom = Eatom (QName q,($startpos(q),$endpos(q)))
    in Call (atom,e2,s)
  }
*/
| a=atomic              { Ast.Eatom a  }
| e=dsl_list(task_expr) { Ast.Elist e  }
| e=tuple(task_exprs)   { Ast.Etuple e }

inspect_expr:
  i=INSPECT e=task_expr FOR c=inspect_clauses END {
   Ast.Insp(e,c,($startpos(i),$endpos(i))) }

inspect_clauses:
  c=semi_list(inspect_clause) { c }

inspect_clause:
  p=task_pattern g=option(guard) ARROW e=task_expr { p,g,e }

task_pattern:
  STABLE p=pattern { Ast.Stable p }
| ERROR  p=pattern { Ast.Error  p }

tref_and_tlam:
  f=TASK id=Atom SLASH n=Nat {
    Ast.Tref((None,id),n,($startpos(f),$endpos(f)))
  }
| f=TASK clauses=semi_list(task_clause) e=END {
    Ast.Tlam(clauses,($startpos(f),$endpos(e)))
  }
