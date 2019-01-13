(* Collating function references from expressions *)

let fref (this_mod: string) (vars: Var.Set.t) : Ref.Set.t =
  let open Var in
  let app = function
    FRef ((None,f_name),arity) ->
      Ref.Set.add ((Some this_mod,f_name),arity)
  | FRef ((Some m_name,f_name),arity) ->
      Ref.Set.add ((Some m_name,f_name),arity)
  | VRef _ -> fun acc -> acc
in Var.Set.fold app vars Ref.Set.empty

let mk_env ((m_name,_,kinds,_) : Ast.t) : Ref.env =
  let app map = function
    Ast.Task _ -> map
  | k -> let f_name, arity, vars = Var.fv_kind k in
           let qfun = (Some m_name, f_name), arity
         in Ref.Map.add qfun (fref m_name vars) map
in List.fold_left app Ref.Map.empty kinds

(* Transitive closure of functional bindings *)

exception Unbound of Ref.t

let dfs env ref : Ref.Set.t =
  let rec dfs ref set =
    if Ref.Set.mem ref set then set
    else let succ = try Ref.Map.find ref env with
                      Not_found -> raise (Unbound ref)
         in Ref.Set.fold dfs succ (Ref.Set.add ref set)
in Ref.Set.remove ref (dfs ref Ref.Set.empty)

let close env =
  let app key _ = Ref.Map.add key (dfs env key)
in Ref.Map.fold app env Ref.Map.empty

(* Collating all dependencies on function names within a lambda *)

let gather env (ref: Ref.Set.t) : Ref.Set.t =
  let closed_env = close env in
  let app ref set =
    let succ = try Ref.Map.find ref closed_env with
                 Not_found -> raise (Unbound ref)
    in Ref.Set.union set succ
in Ref.Set.fold app ref ref

(* Storing the function definitions in a map *)

type def = Ast.kind Ref.Map.t

let store (m_name,_,kinds,_: Ast.t) : def =
  let app acc = function
    Ast.Task (_,_) -> acc
  | Ast.Func ((f_name,arity,clauses),seg) as k ->
      Ref.Map.add ((Some m_name,f_name),arity) k acc
in List.fold_left app Ref.Map.empty kinds

(* Gathering definitions into a module *)

let complete task rset def name : Ast.t =
  let app ref acc =
    try Ref.Map.find ref def :: acc with
      Not_found -> raise (Unbound ref) in
  let open Ast
in name, [], Task task :: Ref.Set.fold app rset [],
   let ((name,_,_),_) = task
   in Exec (Eatom (Var name,Error.dummy),Error.dummy)

(* Generator of task names *)

let gen_task, clr_task =
  let r = ref 0 
in (fun () -> incr r; "t" ^ string_of_int !r), fun () -> r := 0

(* Lambda-lifting *)

let ast = ref None

let mod_name : Ast.t -> string = function (m,_,_,_) -> m
  
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
    let free = List.fold_left Var.fv_lam Var.Set.empty clauses in
    let app (fv,fr) = function
      Var.FRef ((None,fn),a) ->
        fv, Ref.Set.add ((Some (mod_name ast),fn),a) fr
    | Var.FRef ref -> fv, Ref.Set.add ref fr
    | Var.VRef var -> var::fv, fr in
    let fv,fr = 
      Var.Set.fold (fun v a -> app a v) free ([],Ref.Set.empty) in
    let mk_arg v = Eatom (Var v,Error.dummy) in
    let args = List.map mk_arg fv in
    let name = gen_task () in
    let mk_pat v = Patom (Var v, Error.dummy) in
    let env_pat = Ptuple (List.map mk_pat fv) in
    let clauses', mods = lam_clauses clauses in
    let clauses'' =
      List.map (fun (p,g,e) -> (env_pat::p),g,e) clauses' in
    let arity = List.length fv in
    let task = ((name,arity,clauses''),Error.dummy) in
    (try
       let refs = gather (mk_env ast) fr in
       let new_mod = [complete task refs (store ast) name] in
       let lam_ar =
         List.length ((fun (p,_,_) -> p) (List.hd clauses)) in
       let code = Tref((Some name,name),lam_ar+1,Error.dummy) in
       let closure = Etuple [code; Etuple args]
       in closure, new_mod @ mods
    with Unbound r ->
           prerr_endline ("Unbound " ^ Ref.to_string r);
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

| Dist (e,n,s) ->
  (* The constraints are not lambda-lifted *)
    let e',m = lam_exp e in Dist(e',n,s), m

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

let up ((m,s,kinds,ex) as t : Ast.t) : Ast.t * Ast.t list =
  let () = ast := Some t in
  let app (kinds,trees) (k: Ast.kind) =
    try
      let k',t = lam k in k'::kinds, t @ trees
    with No_lambda -> k::kinds, trees in
  let kinds,trees = List.fold_left app ([],[]) kinds
in (m,s,kinds,ex), trees
 
(* Testing *)

(* Generator of function names *)

let gen_fun, clr_fun =
  let r = ref 0 
in (fun () -> incr r; "f" ^ string_of_int !r), fun () -> r := 0
