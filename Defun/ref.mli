(* Function references *)

(* Module [Var] defines a type [t] modelling both variable references
   and function references. Here, we define [t] as only the latter
   kind because, in Erlang, only functions can be defined at
   top-level, therefore the variables free in a function definition
   are always functional (what we call here \emph{function 
   references}). *)

type t = Ast.qname * Ast.arity

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

(* An environment is a map from function references ([t]) to sets of
   function references ([Set.t]). *)

type env = Set.t Map.t

(* Pretty-printing *)

val to_string : t -> string
val print_env : out_channel -> env -> unit
