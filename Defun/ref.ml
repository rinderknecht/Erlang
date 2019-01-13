(* Function references *)

type t = Ast.qname * Ast.arity

module Ord = struct
  (* Contorted because type definitions are recursive. *)
  type t' = t
  type t = t'
  let compare = Pervasives.compare
end

module Map = Map.Make(Ord)
module Set = Set.Make(Ord)

type env = Set.t Map.t

(* Pretty-printing of function references. *)

let to_string = function
  ((Some m_name,f_name),arity) ->
    "fun " ^ m_name ^ ":" ^ f_name ^ "/" ^ string_of_int arity
| ((None,f_name),arity) ->
    "fun " ^ f_name ^ "/" ^ string_of_int arity

(* Testing bindings *)

let print_refs (out: out_channel) =
  Set.iter (fun r -> output_string out (to_string r ^ "; "))

let print_env (out: out_channel) =
  let print key value =
    output_string out (to_string key);
    output_string out " -> ";
    print_refs out value;
    output_string out "\n"
in Map.iter print
