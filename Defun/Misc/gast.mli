(* Abstract Syntax Tree for the workflows *)

type task = T and func = F

type t = kind list

and kind =
  Task of name * task clause list
| Func of name * func clause list

and name = string

and 'a clause = pattern list * guard option * 'a expr list

and pattern =
  Patom  of (basic * Error.seg)
| Plist  of pattern list
| Ptuple of pattern list
| Palias of pattern * pattern * Error.seg

and basic =
  Var    of name
| Nat    of int
| Float  of string
| Atom   of name
| String of string
| True
| False
| Wild

and guard = func expr * Error.seg

and _ expr =
  (* Variants for the purely functional subexpressions *)
  Let : pattern * func expr * Error.seg -> 'a expr

| OrElse  : 'a expr * 'a expr * Error.seg -> 'a expr
| AndAlso : 'a expr * 'a expr * Error.seg -> 'a expr
| Not     : 'a expr * Error.seg -> 'a expr

| Leq : 'a expr * 'a expr * Error.seg -> 'a expr
| Lt  : 'a expr * 'a expr * Error.seg -> 'a expr
| Geq : 'a expr * 'a expr * Error.seg -> 'a expr
| Gt  : 'a expr * 'a expr * Error.seg -> 'a expr
| Neq : 'a expr * 'a expr * Error.seg -> 'a expr
| Eq  : 'a expr * 'a expr * Error.seg -> 'a expr

| App : func expr * func expr * Error.seg -> 'a expr

| Plus   : func expr * func expr * Error.seg -> 'a expr
| Minus  : func expr * func expr * Error.seg -> 'a expr
| Ratio  : func expr * func expr * Error.seg -> 'a expr
| Mult   : func expr * func expr * Error.seg -> 'a expr
| Div    : func expr * func expr * Error.seg -> 'a expr
| Rem    : func expr * func expr * Error.seg -> 'a expr
| Uplus  : 'a expr * Error.seg -> 'a expr
| Uminus : 'a expr * Error.seg -> 'a expr

(*| If   : 'b if_clause list * Error.seg -> 'a expr*)
| If   : Error.seg -> 'a expr

| Case : func expr * case_clause list * Error.seg -> 'a expr

| TryOf : func expr list * try_clause list * try_catch 
          * Error.seg -> 'a expr
| Try   : func expr list * try_catch * Error.seg -> 'a expr
| Throw : func expr * Error.seg -> 'a expr

| Ref    : name * arity * Error.seg -> 'a expr
| Lambda : func clause list * Error.seg -> 'a expr
| Call   : func expr * func expr list * Error.seg -> 'a expr

| Eatom  : (basic * Error.seg) -> 'a expr
| Elist  : 'a expr list -> 'a expr
| Etuple : 'a expr list -> 'a expr

  (* Variants for the task subexpressions *)

| Iter  : task expr * func expr option -> task expr
| Do    : task expr * func expr -> task expr
| Ret   : task expr -> task expr
| Ctrl  : task expr * 'a expr * func expr option -> task expr
| Or    : task expr * task expr -> task expr
| Par   : task expr * task expr -> task expr
| Exec  : task expr -> task expr
| Cont  : func expr -> task expr
| Delay : func expr -> task expr
| Retry : task expr * func expr -> task expr
| Dist  : task expr * nodes * 'a constraints -> task expr
| Raise : task expr -> task expr
| Tref  : name * arity * Error.seg -> task expr
| Tlam  : task clause list * Error.seg -> task expr
| Bind  : task expr * task expr -> task expr
| Drop  : task expr * task expr -> task expr
| Insp  : task expr * inspect_clause list -> task expr

and 'a if_clause = 'a expr * 'a expr list

and case_clause = pattern * guard option * func expr list

and try_catch =
  Catch of try_clause list * after option * Error.seg
| After of after

and try_clause = case_clause

and after = func expr list * Error.seg

and arity = int

and nodes = One | Many

and 'a constraints = basic list * 'a selector option

and 'a selector = 'a expr

and inspect_clause = task_pattern * guard option * task expr list

and task_pattern = 
  Nothing
| Unstable  of pattern
| Stable    of pattern
| Exception of pattern
