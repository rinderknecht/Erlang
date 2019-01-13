(* Abstract Syntax Tree for Rea *)

type t = name * type_spec list * kind list

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

and entity = name * arity * clause list

and kind = Task of entity * Error.seg
         | Func of entity * Error.seg

and name = string

and qname = string option * name   (* Optional module name *)

and clause = pattern list * guard option * expr

and pattern = Patom  of (basic * Error.seg)
            | Plist  of (pattern list * pattern option)
            | Ptuple of pattern list
            | Palias of pattern * pattern * Error.seg

and basic = Var    of name
          | Nat    of int
          | Float  of string
          | Atom   of name
          | String of string
          | QName  of (name * name)
          | True
          | False
          | Wild

and guard = expr * Error.seg

and expr =
  (* Variants for the primitive tasks *)
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
| Lambda of clause list * Error.seg
| Call   of expr * expr list * Error.seg

| Eatom  of (basic * Error.seg)
| Elist  of (expr list * expr option)
| Etuple of expr list

  (* Variants for the general tasks *)

| Iter  of expr * expr option * Error.seg
| Do    of expr * expr * expr * Error.seg
| Ret   of expr * Error.seg
| Ctrl  of expr * expr * expr option * Error.seg
| Par   of expr * expr * Error.seg
| Any   of expr * expr * Error.seg
| Dist  of expr * nodes * constraints * Error.seg
| Exec  of expr * Error.seg
| Cont  of expr * Error.seg
| Delay of expr * Error.seg
| Retry of expr * expr * Error.seg
| Raise of expr * Error.seg
| Tref  of qname * arity * Error.seg
| Tlam  of clause list * Error.seg
| Bind  of expr * expr * Error.seg
| Drop  of expr * expr * Error.seg
| Insp  of expr * inspect_clause list * Error.seg

and if_clause = expr * expr * Error.seg

and case_clause = pattern * guard option * expr

and try_clause = case_clause

and catch_clause = pattern * expr

and arity = int

and nodes = One | Many

and constraints = (expr list * expr option) * selector option

and selector = expr

and inspect_clause = task_pattern * guard option * expr

and task_pattern = Stable of pattern
                 | Error  of pattern
