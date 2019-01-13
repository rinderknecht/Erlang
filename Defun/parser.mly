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
| QName (_,_,s) ->
    Error.signal "No qualified names allowed in pattern." s

%}

/* Keywords */

%token ANDALSO CASE CATCH DIV END FALSE 
%token FUN IF NOT OF ORELSE REM THROW TRUE TRY WHEN
%token MODULE COMPILE EXPORT_ALL TYPE
%token ANY_TYPE FLOAT INTEGER CHAR STRING BOOLEAN LIST

/* Symbols */

%token COMMA     /* ,   */
%token SEMI      /* ;   */
%token DOT       /* .   */
%token COLON     /* :   */

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

/* End-of-file Sentinel */

%token EOF

/* Entries */

%start main
%type <Ast.t> main

%%

/* Grammar */

main:
  m=module_name compile_opt t=type_spec* d=fun_def* EOF {
    m, t, d
  }

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

fun_def:
  d=semi_list(named_fun_clause) DOT {
    match d with
                 [] -> assert false
    | (n,_,((p,_,_) as c))::more ->
      let e = "Inconsistent function name." in
        let a = List.length p
      in n, a, c :: extract e n a more
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
(* | q=QName  { Ast.QName q,  ($startpos(q),$endpos(q)) } *)
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

| a1=Atom c=COLON a2=Atom v=args {
    let open Ast in
      let s,e' = v in
      let fname = Eatom (Atom a2,($startpos(a2),$endpos(a2))) in
      let mname = Eatom (Atom a1,($startpos(a1),$endpos(a1)))
    in Call (QName (mname,fname,($startpos(c),$endpos(c))),e',s)
  }

| a=Atom c=COLON LPAR e=expr RPAR v=args {
    let open Ast in
      let s,e' = v in 
      let mname = Eatom (Atom a,($startpos(a),$endpos(a)))
    in Call (QName (mname,e,($startpos(c),$endpos(c))),e',s)
  }

| LPAR e=expr RPAR c=COLON f=Atom v=args {
    let open Ast in
      let s,e' = v in
      let atom = Eatom (Atom f,($startpos(f),$endpos(f)))
    in Call (QName (e,atom,($startpos(c),$endpos(c))),e',s)
  }

| LPAR e1=expr RPAR c=COLON LPAR e2=expr RPAR v=args {
    let open Ast in
      let s,e' = v
    in Call (QName (e1,e2,($startpos(c),$endpos(c))),e',s)
  }

| f=Atom a=args {
    let open Ast in
      let s,e2 = a in
      let atom = Eatom (Atom f,($startpos(f),$endpos(f)))
    in Call(atom,e2,s)
  }

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

| f=FUN m=Atom COLON id=Atom SLASH n=Nat {
    Ast.Ref((Some m,id),n,($startpos(f),$endpos(f))) }

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
