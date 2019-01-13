(* Collating free variables from expressions *)

type var = FRef of (Ast.qname * Ast.arity)
         | VRef of Ast.name

module VarOrd = struct
  type t = var
  let compare = Pervasives.compare
end

module VarSet = Set.Make(VarOrd)

let rec fv_exp exp =
  List.fold_left (fun vars e -> VarSet.union vars (free e))
                 VarSet.empty exp

and fv_case vars (p,_,e) =
  VarSet.union vars (VarSet.diff (free e) (fv_p p))

and fv_catch vars (p,e) =
  VarSet.union vars (VarSet.diff (free e) (fv_p p))

and fv_patt patt =
  List.fold_left (fun v p -> VarSet.union v (fv_p p))
                 VarSet.empty patt

and fv_p = let open Ast in function
  Patom (b,_) -> fv_bas b
| Plist (patt, None) | Ptuple patt ->  fv_patt patt
| Plist (patt, Some p) -> fv_patt (p::patt)
| Palias (p1,p2,_) -> VarSet.union (fv_p p1) (fv_p p2)

and fv_bas = let open Ast in function
  Var v -> VarSet.singleton (VRef v)
|     _ -> VarSet.empty

and fv_lam vars (patt,_,e) =
  VarSet.union vars (VarSet.diff (free e) (fv_patt patt))

and free : Ast.expr -> VarSet.t = let open Ast in function
  (* Primitive tasks *)
  LetIn (p,e1,e2,_) ->
    VarSet.diff (VarSet.union (free e1) (free e2)) (fv_p p)

| Not (e,_) | Uplus (e,_) | Uminus (e,_) | Throw (e,_) -> free e

| OrElse (e1,e2,_) | AndAlso (e1,e2,_)
| Leq (e1,e2,_) | Lt (e1,e2,_) | Geq (e1,e2,_) | Gt (e1,e2,_)
| Neq (e1,e2,_) | Eq (e1,e2,_) | App (e1,e2,_)
| Plus (e1,e2,_) | Minus (e1,e2,_) | Ratio (e1,e2,_)
| Mult (e1,e2,_) | Div (e1,e2,_) | Rem (e1,e2,_) ->
    VarSet.union (free e1) (free e2)

| If (cases,_) ->
    let collect v (e1,e2,_) =
      VarSet.union v (VarSet.union (free e1) (free e2))
    in List.fold_left collect VarSet.empty cases

| Case (e,cases,_) -> List.fold_left fv_case (free e) cases

| Try (e, of_cases, catch_cases,_) ->
    (List.fold_left fv_catch
      (List.fold_left fv_case (free e) of_cases) catch_cases)

| Ref (q,a,_) -> VarSet.singleton (FRef(q,a))
| Lambda (clauses,_) -> List.fold_left fv_lam VarSet.empty clauses

| Call (Eatom (Atom n,_),exp,_) ->
    VarSet.add (FRef ((None,n),List.length exp)) (fv_exp exp)

| Call (Eatom (QName (m,n),_),exp,_) ->
    VarSet.add (FRef ((Some m,n),List.length exp)) (fv_exp exp)

| Call (e,exp,_) -> VarSet.union (free e) (fv_exp exp)

| Eatom (b,_) -> fv_bas b
| Elist (exp, None) | Etuple exp -> fv_exp exp
| Elist (exp, Some e) -> fv_exp (e::exp)

(* General Tasks *)
| Iter (e, None,_) | Exec (e,_) | Cont (e,_) | Delay (e,_)
| Raise (e,_) | Ret (e,_) -> free e

| Iter (e1, Some e2,_)
| Par (e1,e2,_) | Any (e1,e2,_) | Retry (e1,e2,_) | Bind (e1,e2,_)
| Drop (e1,e2,_) | Ctrl (e1,e2,None,_) ->
   VarSet.union (free e1) (free e2)

| Do (t1,t2,e,_) -> 
    VarSet.union (VarSet.union (free t1) (free t2)) (free e)

| Ctrl (e1,e2,Some e3,_) ->
    VarSet.union (VarSet.union (free e1) (free e2)) (free e3)

| Dist (e,_,c,_) -> VarSet.union (free e) (fv_cons c)

| Tref (q,a,_) -> VarSet.singleton (FRef(q,a))

| Tlam (clauses,_) -> List.fold_left fv_lam VarSet.empty clauses

| Insp (e,clauses,_) -> List.fold_left fv_insp (free e) clauses

and fv_cons = function
  elist, None   -> fv_elist elist
| elist, Some e -> VarSet.union (fv_elist elist) (free e)

and fv_elist = function
  exp, None   -> fv_exp exp
| exp, Some e -> fv_exp (e::exp)

and fv_insp vars = let open Ast in function
  Stable p,_,e | Error p,_,e ->
   VarSet.union vars (VarSet.diff (free e) (fv_p p))

let fv_kind : Ast.kind -> Ast.name * Ast.arity * VarSet.t = 
  let open Ast in function
  Task ((name, arity, clauses),_) 
| Func ((name, arity, clauses),_) ->
    name, arity, VarSet.remove (FRef ((None,name),arity))
                      (List.fold_left fv_lam VarSet.empty clauses)

let fv_kinds : Ast.kind list -> (Ast.name * var list) list =
  List.map (fun k -> let n,_,v = fv_kind k in n, VarSet.elements v)

(* Collating function references from expressions *)

type m_name = string
type f_name = string
type fref = Ast.qname * Ast.arity

module RefOrd = struct
  type t = fref
  let compare = Pervasives.compare
end

module RefMap = Map.Make(RefOrd)
module RefSet = Set.Make(RefOrd)
type env = RefSet.t RefMap.t

let string_of_fref = function
  ((Some m_name,f_name),arity) ->
    "fun " ^ m_name ^ ":" ^ f_name ^ "/" ^ string_of_int arity
| ((None,f_name),arity) ->
    "fun " ^ f_name ^ "/" ^ string_of_int arity

let fref (this_mod: string) (vars: VarSet.t) : RefSet.t =
  let app = function
    FRef ((None,f_name),arity) ->
      RefSet.add ((Some this_mod,f_name),arity)
  | FRef ((Some m_name,f_name),arity) ->
      RefSet.add ((Some m_name,f_name),arity)
  | VRef _ -> fun acc -> acc
in VarSet.fold app vars RefSet.empty

let mk_env ((m_name,_,kinds) : Ast.t) : env =
  let app map = function
    Ast.Task _ -> map
  | k -> let f_name, arity, vars = fv_kind k in
           let qfun = (Some m_name, f_name), arity
         in RefMap.add qfun (fref m_name vars) map
in List.fold_left app RefMap.empty kinds

(* Transitive closure of functional bindings *)

exception Unbound of fref

let dfs env ref : RefSet.t =
  let rec dfs ref set =
    if RefSet.mem ref set then set
    else let succ = try RefMap.find ref env with
                      Not_found -> raise (Unbound ref)
         in RefSet.fold dfs succ (RefSet.add ref set)
in RefSet.remove ref (dfs ref RefSet.empty)

let close env =
  let app key _ = RefMap.add key (dfs env key)
in RefMap.fold app env RefMap.empty

(* Collating all dependencies on function names within a lambda *)

let gather env (ref: RefSet.t) : RefSet.t =
  let closed_env = close env in
  let app ref set =
    let succ = try RefMap.find ref closed_env with
                 Not_found -> raise (Unbound ref)
    in RefSet.union set succ
in RefSet.fold app ref ref

(* Storing the function definitions in a map *)

type def = Ast.kind RefMap.t

let store (m_name,_,kinds: Ast.t) : def =
  let app acc = function
    Ast.Task (_,_) -> acc
  | Ast.Func ((f_name,arity,clauses),seg) as k ->
      RefMap.add ((Some m_name,f_name),arity) k acc
in List.fold_left app RefMap.empty kinds

(* Gathering definitions into a module *)

let complete task rset def name : Ast.t =
  let app ref acc =
    try RefMap.find ref def :: acc with
      Not_found -> raise (Unbound ref)
in name, [], task :: RefSet.fold app rset []

(* Generator of task names *)

let gen_task, clr_task =
  let r = ref 0 
in (fun () -> incr r; "t" ^ string_of_int !r), fun () -> r := 0

(* Lambda-lifting *)

let ast = ref None

let mod_name : Ast.t -> string = function (m,_,_) -> m
  
exception No_AST

let rec mk_bin const e1 e2 =
  let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
in const e1' e2', m1 @ m2
  
(* val lam_exp : Ast.expr -> Ast.expr * Ast.t list *)
  
and lam_clauses clauses : Ast.clause list * Ast.t list =
  let app (patterns,guard,expr) (c,d) =
    let e', decl = lam_exp expr
    in (patterns,guard,e')::c, decl @ d
in List.fold_right app clauses ([],[])

and lam_exp = let open Ast in function
  (* Variants for the primitive tasks *)
  LetIn (p,e1,e2,s) ->
    mk_bin (fun e1' e2' -> LetIn(p,e1',e2',s)) e1 e2
| OrElse (e1,e2,s) ->
    mk_bin (fun e1' e2' -> OrElse(e1',e2',s)) e1 e2
| AndAlso (e1,e2,s) ->
    mk_bin (fun e1' e2' -> AndAlso(e1',e2',s)) e1 e2
| Leq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Leq(e1',e2',s)) e1 e2
| Lt (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Lt(e1',e2',s)) e1 e2
| Geq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Geq(e1',e2',s)) e1 e2
| Gt (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Gt(e1',e2',s)) e1 e2
| Neq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Neq(e1',e2',s)) e1 e2
| Eq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Eq(e1',e2',s)) e1 e2
| App (e1,e2,s) ->
    mk_bin (fun e1' e2' -> App(e1',e2',s)) e1 e2
| Plus (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Plus(e1',e2',s)) e1 e2
| Minus (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Minus(e1',e2',s)) e1 e2
| Ratio (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Ratio(e1',e2',s)) e1 e2
| Mult (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Mult(e1',e2',s)) e1 e2
| Div (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Div(e1',e2',s)) e1 e2
| Rem (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Rem(e1',e2',s)) e1 e2

| Not (e,s)    -> let e',m = lam_exp e in Not(e',s), m
| Uplus (e,s)  -> let e',m = lam_exp e in Uplus(e',s), m
| Uminus (e,s) -> let e',m = lam_exp e in Uminus(e',s), m

| If (cases,s) ->
    let app (prev,mods) (e1,e2,s) =
      let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
      in (e1',e2',s)::prev, m1 @ m2 @ mods in
    let cases',m = List.fold_left app ([],[]) cases
    in If(cases',s), m

| Case (e,cases,s) ->
    let app (prev,mods) (p,g,e) =
      let e',m = lam_exp e in (p,g,e')::prev, m @ mods in
    let cases', m1 = List.fold_left app ([],[]) cases in
    let e',m2 = lam_exp e
    in Case(e',cases',s), m1 @ m2

| Try (e,cases,catches,s) ->
    let app1 (prev,mods) (p,g,e) =
      let e',m = lam_exp e in (p,g,e')::prev, m @ mods in
    let cases', m1 = List.fold_left app1 ([],[]) cases in
    let app2 (prev,mods) (p,e) =
      let e',m = lam_exp e in (p,e')::prev, m @ mods in
    let catches', m2 = List.fold_left app2 ([],[]) catches in
    let e',m3 = lam_exp e
    in Try(e',cases',catches',s), m1 @ m2 @ m3
   
| Throw (e,s) -> let e',m = lam_exp e in Throw(e',s), m

| Ref (_,_,_) as e -> e, []
| Lambda (_,_) as e -> e, []

| Call (e,es,s) ->
    let e',m1 = lam_exp e in
    let app (prev,mods) e = 
      let e',m = lam_exp e in e'::prev, m @ mods in
    let es',m2 = List.fold_left app ([],[]) es
    in Call(e',es',s), m1 @ m2 

| Eatom _ as e -> e, []

| Elist (es, None) ->
    let app (prev,mods) e = 
      let e',m = lam_exp e in e'::prev, m @ mods in
    let es',m = List.fold_left app ([],[]) es
    in Elist(es',None), m

| Elist (es, Some e) ->
    let e',m1 = lam_exp e in
    let app (prev,mods) e = 
      let e',m = lam_exp e in e'::prev, m @ mods in
    let es',m2 = List.fold_left app ([],[]) es
    in Elist(es', Some e'), m1 @ m2 

| Etuple es ->
    let app (prev,mods) e = 
      let e',m = lam_exp e in e'::prev, m @ mods in
    let es',m = List.fold_left app ([],[]) es
    in Etuple es', m

  (* Variants for the general tasks *)

  (* Primitive tasks *)
| Ret (e,s) ->  let e',m = lam_exp e in Ret(e',s), m
| Cont (e,s) -> let e',m = lam_exp e in Ret(e',s), m
| Delay (e,s) -> let e',m = lam_exp e in Ret(e',s), m
| Raise (e,s) -> let e',m = lam_exp e in Ret(e',s), m
| Tref (_,_,_) as t -> t, []
| Tlam (clauses, s) ->
    let ast = match !ast with Some t -> t 
                            | None -> raise No_AST in
    let free = List.fold_left fv_lam VarSet.empty clauses in
    let app (fv,fr) = function
      FRef ((None,fn),a) ->
        fv, RefSet.add ((Some (mod_name ast),fn),a) fr
    | FRef ref -> fv, RefSet.add ref fr
    | VRef var -> var::fv, fr in
    let fv,fr = 
      VarSet.fold (fun v a -> app a v) free ([],RefSet.empty) in
    let mk_arg v = Eatom (Var v,Error.dummy) in
    let args = List.map mk_arg fv in
    let name = gen_task () in
    let mk_pat v = Patom (Var v, Error.dummy) in
    let env_pat = Ptuple (List.map mk_pat fv) in
    let clauses', mods = lam_clauses clauses in
    let clauses'' =
      List.map (fun (p,g,e) -> (env_pat::p),g,e) clauses' in
    let arity = List.length fv in
    let task = Task ((name,arity,clauses''),Error.dummy) in
    (try
       let refs = gather (mk_env ast) fr in
       let new_mod = [complete task refs (store ast) name] in
       let lam_ar =
         List.length ((fun (p,_,_) -> p) (List.hd clauses)) in
       let code = Tref((Some name,name),lam_ar+1,Error.dummy) in
       let closure = Etuple [code; Etuple args]
       in closure, new_mod @ mods
    with Unbound r ->
           prerr_endline ("Unbound " ^ string_of_fref r);
           exit 1)

  (* Basic combinators *)
| Iter (e,None,s) -> let e',m = lam_exp e in Iter(e',None,s), m

| Iter (e1,Some e2,s) ->
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Iter(e1',Some e2',s), m1 @ m2

| Ctrl (e1,e2,acc,s) ->
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Ctrl(e1',e2',acc,s), m1 @ m2

| Par (e1,e2,s) ->
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Par(e1',e2',s), m1 @ m2

| Dist (e,nodes,cons,s) ->
  (* The constraints are not lambda-lifted *)
  let e',m = lam_exp e in Dist(e',nodes,cons,s), m

| Exec (e,s) -> let e',m = lam_exp e in Exec(e',s), m

| Bind (e1,e2,s) -> 
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Bind(e1',e2',s), m1 @ m2

  (* Mixed combinators *)
| Insp (e,insp,s) ->
    let e',m1 = lam_exp e in
      let app (prev,mods) (p,g,e) =
        let e',m = lam_exp e in (p,g,e')::prev, m @ mods in
      let insp', m2 = List.fold_left app ([],[]) insp
    in Insp(e',insp',s), m1 @ m2

| Any (e1,e2,s) ->
    let e2',m = lam_exp e2 in Any(e1,e2',s), m

  (* Super combinators *)
| Drop (e1,e2,s) ->
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Drop(e1',e2',s), m1 @ m2

| Do (e1,e2,e3,s) ->
   let e1',m1 = lam_exp e1 and e2',m2 = lam_exp e2
   in Do(e1',e2',e3,s), m1 @ m2  

| Retry (e1,e2,s) ->
   let e1',m = lam_exp e1 in Retry(e1',e2,s), m

exception No_lambda

let lam_ent (name,arity,clauses) : Ast.entity * Ast.t list =
  match lam_clauses clauses with
    clauses',   [] -> raise No_lambda
  | clauses', decl -> (name,arity,clauses'), decl

let lam : Ast.kind -> Ast.kind * Ast.t list =
  let open Ast in function
  Func (e,s) -> let e1,m = lam_ent e in Func(e1,s),m
| Task (e,s) -> let e1,m = lam_ent e in Task(e1,s),m

let up ((m,s,kinds) as t : Ast.t) : Ast.t * Ast.t list =
  let () = ast := Some t in
  let app (kinds,trees) (k: Ast.kind) =
    try
      let k',t = lam k in k'::kinds, t @ trees
    with No_lambda -> k::kinds, trees in
  let kinds,trees = List.fold_left app ([],[]) kinds
in (m,s,kinds), trees
 
(* Testing *)

(* Testing bindings *)

let print_refs (out: out_channel) =
  RefSet.iter (fun r -> output_string out (string_of_fref r ^ "; "))

let print_env (out: out_channel) =
  let print key value =
    output_string out (string_of_fref key);
    output_string out " -> ";
    print_refs out value;
    output_string out "\n"
in RefMap.iter print

(* Testing free variables *)

let string_of_var : var -> string = function
  FRef   ((None,f), a) -> "fun " ^ f ^ "/" ^ string_of_int a
| FRef ((Some m,f), a) -> "fun " ^ m ^ ":" ^ f ^ "/" ^ string_of_int a
|               VRef s -> s

let rec string_of_vars : var list -> string = function
       [] -> ""
|     [v] -> string_of_var v
| v::more -> string_of_var v ^ ", " ^ string_of_vars more

let print_fv (out: out_channel) (kinds: Ast.kind list) : unit =
  let rec print = function
             [] -> ()
  | (n,v)::more -> 
      output_string out ("Free variables of " ^ n ^ ":\n");
      output_string out (string_of_vars v);
      output_string out "\n";
      print more
in print (fv_kinds kinds)

(* Generator of function names *)

let gen_fun, clr_fun =
  let r = ref 0 
in (fun () -> incr r; "f" ^ string_of_int !r), fun () -> r := 0
