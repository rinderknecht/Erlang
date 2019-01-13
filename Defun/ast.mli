(* Abstract Syntax Tree for Erlang *)

type t = name * type_spec list * fun_def list

(* Type specifications *)

and type_spec = poly_type * type_expr

and type_expr = Texpr of type__ | Tsum of val_const list

and type__ = Any | Float | Int | Char | Str | Bool
           | Fun   of fun_type
           | List  of type__ 
           | Tuple of type__ list
           | Poly  of poly_type

and poly_type = name * type_var list

and fun_type = type__ list * type__

and type_var = name

and val_const = name * type__ list

and fun_def = name * arity * fun_clause list

and name = string

and qname = string option * name   (* Optional module name *)

and fun_clause = pattern list * guard option * expr

and pattern = Patom  of (basic * Error.seg)
            | Plist  of (pattern list * pattern option)
            | Ptuple of pattern list
            | Palias of pattern * pattern * Error.seg

and basic = Var    of name
          | Nat    of int
          | Float  of string
          | Atom   of name
          | String of string
          | True
          | False
          | Wild

and guard = expr * Error.seg

and expr =
  LetIn of pattern * expr * expr * Error.seg

| OrElse  of expr * expr * Error.seg
| AndAlso of expr * expr * Error.seg
| Not     of expr * Error.seg

| Leq of expr * expr * Error.seg
| Lt  of expr * expr * Error.seg
| Geq of expr * expr * Error.seg
| Gt  of expr * expr * Error.seg
| Neq of expr * expr * Error.seg
| Eq  of expr * expr * Error.seg

| App of expr * expr * Error.seg

| Plus   of expr * expr * Error.seg
| Minus  of expr * expr * Error.seg
| Ratio  of expr * expr * Error.seg
| Mult   of expr * expr * Error.seg
| Div    of expr * expr * Error.seg
| Rem    of expr * expr * Error.seg
| Uplus  of expr * Error.seg
| Uminus of expr * Error.seg

| If    of if_clause list * Error.seg
| Case  of expr * case_clause list * Error.seg
| Try   of expr * try_clause list * catch_clause list
           * Error.seg
| Throw of expr * Error.seg

| Ref    of qname * arity * Error.seg
| Lambda of fun_clause list * Error.seg
| Call   of expr * expr list * Error.seg

| Eatom  of (basic * Error.seg)
| Elist  of (expr list * expr option)
| Etuple of expr list
| QName  of (expr * expr * Error.seg)

and if_clause = expr * expr * Error.seg

and case_clause = pattern * guard option * expr

and try_clause = case_clause

and catch_clause = pattern * expr

and arity = int
