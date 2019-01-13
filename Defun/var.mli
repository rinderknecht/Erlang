(* Collating free variables *)

(* The type [t] is inhabited exclusively by function references,
   \emph{e.g.}, [fun f/2], and variable references, \emph{e.g.},
   [V]. In other functional languages, like OCaml, there is only one
   kind of variable, which are only distinguished by their type (being
   functional or not), but, in Erlang, variables denoting functions
   are lexically distinguished. *)

type t = FRef of Ref.t | VRef of Ast.name

module Set : Set.S with type elt = t

(* When [e] is an expression, the call [free e] is evaluated into the
   set of references free in [e]. *)

val free : Ast.expr -> Set.t

(* When [d] is a function definition, the call [fv_fun_def d] is
   evaluated into [d] where the body has been replaced by the free
   references present in [d]. *)

val fv_fun_def : Ast.fun_def -> Ast.name * Ast.arity * Set.t

(* When [v] is a set of references and [c] a function clause, the call
   [fv_lam v c] is evaluated into the set of references free in [c],
   that is, the set of the references free in the body which are not
   bound in the pattern of the clause. *)

val fv_lam : Set.t -> Ast.fun_clause -> Set.t

(* The call [print_fv c d], where [c] is a channel out and [d] a list
  of function definitions, results in pretty-printing [d] in [c]. *)

val print_fv : out_channel -> Ast.fun_def list -> unit
