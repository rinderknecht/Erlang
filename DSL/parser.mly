%{
%}

/* Keywords */

%token AFTER ANDALSO BEGIN CASE CATCH DIV END FALSE 
%token FUN IF NOT OF ORELSE REM THROW TRUE TRY WHEN

/* Symbols */

%token COMMA     /* ,   */
%token SEMI      /* :   */
%token COLON     /* :   */
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

/* Literals */

%token <string> Var
%token <string> String
%token <string> Atom
%token <int> Nat
%token <string> Float

/* Sentinel */

%token EOF

/* Entrance points */

%start main
%type <unit> main

%%

/* Grammar */

main: fun_def* EOF {}

fun_def:
  semi_list(named_fun_clause) DOT {}

semi_list(Item):
  Item {}
| Item SEMI semi_list(Item) {}

named_fun_clause:
  Atom fun_clause {}

fun_clause:
  LPAR option(patterns) RPAR option(guard) ARROW exprs {}

patterns:
  pattern {}
| pattern COMMA patterns {}

pattern:
  atomic {}
| dsl_list(pattern) {}
| tuple(patterns) {}

atomic:
  Var {}
| Nat {}
| Float {}
| Atom {}
| String {}
| TRUE {}
| FALSE {}
| Wild {}

dsl_list(Item):
  LBRACKET RBRACKET {}
| LBRACKET Item tail(Item) {}

tail(Item):
  RBRACKET {}
| MID Item RBRACKET {}
| COMMA Item tail(Item) {}

tuple(Items):
  LBRACE option(Items) RBRACE {}

guard:
  WHEN expr_150 {}

exprs:
  expr {}
| expr COMMA exprs {}

expr:
  expr_750 DEF expr {}
| expr_150 {}

expr_150:
  expr_160 ORELSE expr_150 {}
| expr_160 {}

expr_160:
  expr_200 ANDALSO expr_160 {}
| expr_200 {}

expr_200:
  expr_300 comp_op expr_300 {}
| expr_300 {}

expr_300:
  expr_400 APPEND expr_300 {}
| expr_400 {}

expr_400:
  expr_400 add_op expr_500 {}
| expr_500 {}

expr_500:
  expr_500 mult_op expr_600 {}
| expr_600 {}

expr_600:
  prefix_op expr_700 {}
| expr_700 {}

prefix_op:
  PLUS {}
| MINUS {}
| NOT {}

expr_700:
  LPAR expr RPAR {}
| BEGIN exprs END {}
| if_expr {}
| case_expr {}
| fun_expr {}
| try_expr {}
| THROW LPAR expr RPAR {}
| expr_750 {}

expr_750:
  fun_call {}
| atomic {}
| dsl_list(expr) {}
| tuple(exprs) {}

fun_call: expr_700 args {}

if_expr: IF semi_list(if_clause) END {}

if_clause: expr_150 ARROW exprs {}

case_expr: CASE expr OF case_clauses END {}

case_clauses:
  semi_list(case_clause) {}

case_clause: pattern option(guard) ARROW exprs {}

fun_expr:
  FUN Atom SLASH Nat {}
| FUN Atom COLON Atom SLASH Nat {}
| FUN semi_list(fun_clause) END {}

try_expr:
  TRY exprs OF try_clauses try_catch {}
| TRY exprs try_catch {}

try_catch:
  CATCH try_clauses END {}
| CATCH try_clauses AFTER exprs END {}
| AFTER exprs END {}

try_clauses:
  semi_list(case_clause) {}

args:
  LPAR option(exprs) RPAR {}

add_op:
  PLUS {}
| MINUS {}

mult_op:
  SLASH {}
| TIMES {}
| DIV {}
| REM {}

comp_op:
  LEQ {}
| LT {}
| GEQ {}
| GT {}
| NEQ {}
| EQ {}

/*
try_clauses:
  semi_list(try_clause) {}

try_clause:
  expr option(guard) ARROW exprs {}
| Atom COLON expr option(guard) ARROW exprs {}
| Var COLON expr option(guard) ARROW exprs {}
*/
