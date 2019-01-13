(* Collating free variables *)

type var = FRef of (Ast.qname * Ast.arity)
         | VRef of Ast.name

module VarSet : Set.S with type elt = var

val free : Ast.expr -> VarSet.t

type m_name = string
type f_name = string
type fref = Ast.qname * Ast.arity

module RefMap : Map.S with type key = fref
module RefSet : Set.S with type elt = fref
type env = RefSet.t RefMap.t

val mk_env : Ast.t -> env

exception Unbound of fref

val close : env -> env (* May raise [Unbound] *)

val up : Ast.t -> Ast.t * Ast.t list

val string_of_fref : fref -> string
val print_env : out_channel -> env -> unit
val print_fv : out_channel -> Ast.kind list -> unit

