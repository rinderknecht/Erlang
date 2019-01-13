(* Collating free variables from expressions *)

type t = FRef of (Ast.qname * Ast.arity)
       | VRef of Ast.name

module Ord = struct
  (* Contorted because type definitions are recursive. *)
  type t' = t
  type t = t'
  let compare = Pervasives.compare
end

module Set = Set.Make(Ord)

(* When [e] is a list of expressions, the call [fv_exp e] is evaluated
   into the set of references free in [e]. We have [val fv_exp :
   Ast.expr list -> Set.t]. *)

let rec fv_exp exp =
  List.fold_left (fun vars e -> Set.union vars (free e))
                 Set.empty exp

(* When [v] is a set of references and [c] a case clause, the call
   [fv_case v c] is evaluated into the union of [v] and the references
   free in [c]. We have [val fv_case : Set.t -> Ast.case_clause ->
   Set.t]. *)

and fv_case vars (p,_,e) =
  Set.union vars (Set.diff (free e) (fv_p p))

(* When [v] is a set of references and [c] is a catch clause, the call
   [fv_catch v c] is evaluated into the union of [v] and the set of
   references free in [c]. We have [val fv_catch : Set.t ->
   Ast.catch_clause -> Set.t]. *)

and fv_catch vars (p,e) =
  Set.union vars (Set.diff (free e) (fv_p p))

(* When [p] is a list of patterns, the call [fv_patt p] is evaluated
   into the set of references free in [p]. We have [val fv_patt :
   Ast.pattern list -> Set.t]. *)

and fv_patt patt =
  List.fold_left (fun v p -> Set.union v (fv_p p)) Set.empty patt

(* When [p] is a pattern, the call [fv_p p] is evaluated into the set
   of references free in [p]. We have [val fv_p : Ast.pattern ->
   Set.t]. *)

and fv_p = let open Ast in function
                       Patom (b,_) -> fv_bas b
| Plist (patt, None) | Ptuple patt -> fv_patt patt
|             Plist (patt, Some p) -> fv_patt (p::patt)
|                 Palias (p1,p2,_) -> Set.union (fv_p p1) (fv_p p2)

(* When [b] is a basic lexical unit, the call [fv_bas b] is evaluated
   into the singleton containing [b] if [b] is a variable, and the
   empty set otherwise. The type [val fv_bas : Ast.basic -> Set.t]. *)

and fv_bas = let open Ast in function
  Var v -> Set.singleton (VRef v)
|     _ -> Set.empty

(* See interface. We have [val fv_lam : Set.t -> Ast.fun_clause ->
   Set.t]. *)

and fv_lam vars (patt,_,e) =
  Set.union vars (Set.diff (free e) (fv_patt patt))

(* See interface. *)

and free : Ast.expr -> Set.t = let open Ast in function
  LetIn (p,e1,e2,_) ->
    Set.diff (Set.union (free e1) (free e2)) (fv_p p)

| Not (e,_) | Uplus (e,_) | Uminus (e,_) | Throw (e,_) -> free e

| OrElse (e1,e2,_) | AndAlso (e1,e2,_)
| Leq (e1,e2,_) | Lt (e1,e2,_) | Geq (e1,e2,_) | Gt (e1,e2,_)
| Neq (e1,e2,_) | Eq (e1,e2,_) | App (e1,e2,_)
| Plus (e1,e2,_) | Minus (e1,e2,_) | Ratio (e1,e2,_)
| Mult (e1,e2,_) | Div (e1,e2,_) | Rem (e1,e2,_) ->
    Set.union (free e1) (free e2)

| If (cases,_) ->
    let collect v (e1,e2,_) =
      Set.union v (Set.union (free e1) (free e2))
    in List.fold_left collect Set.empty cases

| Case (e,cases,_) -> List.fold_left fv_case (free e) cases

| Try (e, of_cases, catch_cases,_) ->
    (List.fold_left fv_catch
      (List.fold_left fv_case (free e) of_cases) catch_cases)

| Ref (q,a,_) -> Set.singleton (FRef(q,a))
| Lambda (clauses,_) -> List.fold_left fv_lam Set.empty clauses

| Call (Eatom (Atom n,_),exp,_) ->
    Set.add (FRef ((None,n),List.length exp)) (fv_exp exp)

| Call (e,exp,_) -> Set.union (free e) (fv_exp exp)

| Eatom (b,_) -> fv_bas b
| Elist (exp, None) | Etuple exp -> fv_exp exp
| Elist (exp, Some e) -> fv_exp (e::exp)
| QName (e1,e2,_) -> Set.union (free e1) (free e2)

(* See interface. *)

let fv_fun_def : Ast.fun_def -> Ast.name * Ast.arity * Set.t =      
  function name, arity, clauses ->
    name, arity, Set.remove (FRef ((None,name),arity))
                            (List.fold_left fv_lam Set.empty clauses)

(* When [defs] is a list of function definitions, the call
   [fv_fun_defs defs] is evaluated into an association list from the
   names of the functions to the references free in their
   corresponding bodies. *)

let fv_fun_defs : Ast.fun_def list -> (Ast.name * t list) list =
  List.map (fun k -> let n,_,v = fv_fun_def k in n, Set.elements v)

(* Testing *)

let to_string : t -> string = function
  FRef   ((None,f), a) -> "fun " ^ f ^ "/" ^ string_of_int a
| FRef ((Some m,f), a) -> "fun " ^ m ^ ":" ^ f ^ "/" ^ string_of_int a
|               VRef s -> s

let rec string_of_list : t list -> string = function
       [] -> ""
|     [v] -> to_string v
| v::more -> to_string v ^ ", " ^ string_of_list more

let print_fv (out: out_channel) (fun_defs: Ast.fun_def list) : unit =
  let rec print = function
             [] -> ()
  | (n,v)::more -> 
      output_string out ("Free variables of " ^ n ^ ":\n");
      output_string out (string_of_list v);
      output_string out "\n";
      print more
in print (fv_fun_defs fun_defs)
