val mk_env : Ast.t -> Ref.env

exception Unbound of Ref.t

val close : Ref.env -> Ref.env (* May raise [Unbound] *)

val up : Ast.t -> Ast.t * Ast.t list


