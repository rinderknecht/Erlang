(* Modular defunctionalisation *)

(* Defunctionalisation is a program transformation resulting in
   equivalent, first-order programs. All function abstractions, also
   known as \emph{lambdas}, are replaced with a pair made of a unique
   tag and the environment captured by that lambda (a list of values);
   dually, each call (this can be optimised) is replaced by a call to
   a special function, with the function called and its arguments as
   arguments, \emph{e.g.}, [F(X)] becomes [app(F,X)]. That function
   [app/2] dispatches all the calls, although only the lambdas are of
   interest. For example, given
   \begin{verbatim}
-module(len).
-compile(export_all).

len(S) ->
  G = fun(F) -> fun(   [],N) -> N;
                   ([_|T],N) -> F(T,N+1)
                end
      end,
  H = fun(F) -> G(fun(X,Y) -> (F(F))(X,Y) end) end,
  (H(H))(S,0).
   \end{verbatim}
   which is the function computing the length of a list [S] written
   without recursion, the transform generates
   \begin{verbatim}
-module(len).
-compile(export_all).
app(tag4, [F]) -> {tag3, F};
app({tag3,F}, [[],N]) -> N;
app({tag3,F}, [[_|T],N]) -> app(F, [T, N + 1]);
app({tag2,G}, [F]) -> app(G, [{tag1, F}]);
app({tag1,F}, [X,Y]) -> app(app(F, [F]), [X, Y]).
len(S) -> G=tag4, H={tag2, G}, app(app(H, [H]), [S, 0]).
   \end{verbatim}
   where the [app/2] function is the dispatcher.

   It is often claimed that defunctionalisation is a whole-program
   transform, because tags must be unique (the set of all lambdas must
   be statically identified) and the dispatch function must also be
   unique (it is a sort of virtual machine). Nevertheless, Erlang
   lacking a static type system, defunctionalisation can be made
   modular, that is, it can preserve separate compilation. Each module
   would have its own dispatcher and, instead of a pair [{tag,Env}],
   we have [{Mod,{tag,Env}}], and the dispatch is first done on the
   module [Mod] and then, as usual, on the tag. For instance, the
   previous example would yield
   \begin{verbatim}
-module(len).
-compile(export_all).

app({tag4,[G]},[F]) ->
    X1=(element(1,G)):app(element(2,G),[{len,{tag3,[F]}}]),
    X1;
app({tag3,[F]},[X,Y]) ->
    X1=(element(1,F)):app(element(2,F),[F]),
    X2=(element(1,X1)):app(element(2,X1),[X,Y]),
    X2;
app({tag2,[]},[F]) -> {len,{tag1,[F]}};
app({tag1,[F]},[[],N]) -> N;
app({tag1,[F]},[[_|T],N]) ->
    X1=(element(1,F)):app(element(2,F),[T,N+1]),
    X1.

len(S) ->
    G={len,{tag2,[]}},
    H={len,{tag4,[G]}},
    X1=(element(1,H)):app(element(2,H),[H]),
    X2=(element(1,X1)):app(element(2,X1),[S,0]),
    X2.
   \end{verbatim}
   It would have been easier to generate nesting patterns like
   \begin{verbatim}
X = begin Y = ..., ..Y.. end, ..X..
   \end{verbatim}
*)

(* Utility functions *)

let id x = x
let compose f g x = f (g x)

(* Variable generator

   We use mutation to generate unique variables, but we do not
   guarantee that they never collide with variables from the program
   being transformed, although that is a necessary condition in
   theory. *)

let gen_var, clr_var =
  let r = ref 0 
in (fun () -> incr r; "V" ^ string_of_int !r), fun () -> r := 0

(* When [vars] is a set of references and [m] the name of the current
   module being transformed, the call [fref m vars] is the set of
   function references in [vars], qualified with [m]. Simply put, we
   filter the values of constructor [Ast.FRef], adding the module
   qualification when needed, and ignore any [Ast.VRef]. *)

let fref (this_mod: string) (vars: Var.Set.t) : Ref.Set.t =
  let open Var in
  let app = function
    FRef ((None,f_name),arity) ->
      Ref.Set.add ((Some this_mod,f_name),arity)
  | FRef r -> Ref.Set.add r
  | VRef _ -> id
in Var.Set.fold app vars Ref.Set.empty

(* When [ast] is an abstract syntax tree, the call [mk_env ast] is
   evaluated into an environment. In other words, the function
   [mk_env] extract the (functional) environment from the AST. *)

let mk_env ((m_name,_,fun_defs) : Ast.t) : Ref.env =
  let app map k =
    let f_name, arity, vars = Var.fv_fun_def k in
    let qfun = (Some m_name, f_name), arity
  in Ref.Map.add qfun (fref m_name vars) map
in List.fold_left app Ref.Map.empty fun_defs

(* Transitive closure of functional bindings.

   When [env] is an environment and [ref] a functional reference, the
   call [dfs env ref] is evaluated into the set of function references
   that can be reached via [ref] in [env]. This is a simple
   depth-first search (hence the function name) and the first unbound
   function reference [r] triggers the exception [Unbound r]. We have
   [val dfs : Ref.Set.t Ref.Map.t -> Ref.Set.elt -> Ref.Set.t]. *)

exception Unbound of Ref.t

let dfs env ref : Ref.Set.t =
  let rec dfs ref set =
    if Ref.Set.mem ref set then set
    else let succ = try Ref.Map.find ref env with
                      Not_found -> raise (Unbound ref)
         in Ref.Set.fold dfs succ (Ref.Set.add ref set)
in Ref.Set.remove ref (dfs ref Ref.Set.empty)

(* When [env] is an environment, the call [close env] is evaluated
   into the smallest, closed superset of [env]. We have
   [val close : Ref.Set.t Ref.Map.t -> Ref.Set.t Ref.Map.t]. *)

let close env =
  let app key _ = Ref.Map.add key (dfs env key)
in Ref.Map.fold app env Ref.Map.empty

(* Storing the function definitions in a map.

   When [ast] is an abstract syntax tree, the call [store ast] is
   evaluated into a map from each function qualified name (including
   arity) to their corresponding definition (type [def]). *)

type def = Ast.fun_def Ref.Map.t

let store (m_name,_,fun_defs: Ast.t) : def =
  let app acc = function (f_name,arity,clauses) as k ->
    Ref.Map.add ((Some m_name,f_name),arity) k acc
in List.fold_left app Ref.Map.empty fun_defs

let out_module = ref ""
  
(* Each function abstraction and function reference is uniquely tagged
   by a string. This is achieved here by mutating a reference ([r]),
   which is used by [mk_tag]. The tags are accessed through the OCaml
   reference [tags], which holds a map from function references to
   pairs of tags and function clauses (denoting a function abstraction
   or reference in the original Erlang program). *)

type tag = string

let reg_ref, reg_lam, get_clauses =
  let tags: (tag * Ast.fun_clause list) Ref.Map.t ref =
      ref Ref.Map.empty in
  let r = ref 0 in
  let mk_tag () = incr r; "tag" ^ string_of_int !r in

  (* Registrar of references

     In theory, function references are a special case of function
     abstractions because [fun foo/n] is equivalent to [fun(X1,...Xn)
     -> foo(X1,...,Xn) end] by eta-expansion, and they are registered
     with [reg_ref], with [val reg_ref : Ref.t -> string]. In
     practice, we would rather prefer that several occurrences of the
     same function reference yield the same tag, therefore, function
     references have a dedicated registrar.

     If the function reference [r] has been registered previously, the
     call [reg_ref r] is evaluated in the registered tag. Otherwise, 

  *)

  let reg_ref (fref: Ref.t) : tag =
    try fst (Ref.Map.find fref !tags) with
      Not_found ->
        let tag = mk_tag () and s = Error.dummy in
        let open Ast in
        let name, arity =
          match fref with
            (Some m, id), a -> QName (Eatom (Atom m,s),
                                      Eatom (Atom id,s),s), a
          |   (None, id), a -> Eatom (Atom id,s), a in
        let rec mk_var l = function
          0 -> l
        | i -> mk_var ((Var ("X" ^ string_of_int i),s)::l) (i-1) in
        let vars = mk_var [] arity in
        let args = List.map (fun v -> Eatom v) vars in
        let body = Call (name,args,s) in
        let pats = Plist (List.map (fun v -> Patom v) vars, None) in
        let clauses = [[Patom (Atom tag, s); pats], None, body]
        in tags := Ref.Map.add fref (tag,clauses) !tags;
           tag in

  (* Registrar of function abstractions

     [val reg_lam : Ast.fun_clause list -> Ast.expr]

  *)

  let reg_lam (clauses: Ast.fun_clause list) : Ast.expr =
    let free = List.fold_left Var.fv_lam Var.Set.empty clauses in
    let app fv = function Var.VRef var -> var::fv | _ -> fv in
    let fv = Var.Set.fold (fun v a -> app a v) free [] in

    let tag = mk_tag () and s = Error.dummy in
    let open Ast in
    let constr = Atom tag, s in

    let mk_pat v = Patom (Var v, s) in
    let clos_pat =
      Ptuple [Patom constr; Plist (List.map mk_pat fv,None)] in
    let extend (p,g,e) = [clos_pat; Plist (p,None)], g, e in
    let clauses' = List.map extend clauses in
    let () =
      tags := Ref.Map.add ((None,tag),0) (tag,clauses') !tags in

    let mk_arg v = Eatom (Var v, s)
    in Etuple [Eatom (Atom !out_module,s); 
               Etuple [Eatom constr;
                       Elist (List.map mk_arg fv,None)]] in

(*
   [val get_clauses : unit -> Ast.fun_clause list]
*)

  let get_clauses () = 
    Ref.Map.fold (fun _ (_,c) acc -> c @ acc) !tags []

in reg_ref, reg_lam, get_clauses

(* The defunctionalisation proper *)

let rec mk_bin const e1 e2 =
  let e1',f1 = defun_exp e1 and e2',f2 = defun_exp e2
in const e1' e2', compose f1 f2

and defun_clauses clauses =
  let app (patterns,guard,expr) =
    let e', f = defun_exp expr in patterns, guard, f e'
in List.map app clauses

and defun_exp = let open Ast in function
  (* Variants for the primitive tasks *)
  LetIn (p,e1,e2,s) ->
    let e1',f1 = defun_exp e1 and e2',f2 = defun_exp e2
    in LetIn (p,e1',f2 e2',s), f1
| OrElse (e1,e2,s) ->
    mk_bin (fun e1' e2' -> OrElse (e1',e2',s)) e1 e2
| AndAlso (e1,e2,s) ->
    mk_bin (fun e1' e2' -> AndAlso (e1',e2',s)) e1 e2
| Leq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Leq (e1',e2',s)) e1 e2
| Lt (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Lt (e1',e2',s)) e1 e2
| Geq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Geq (e1',e2',s)) e1 e2
| Gt (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Gt (e1',e2',s)) e1 e2
| Neq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Neq (e1',e2',s)) e1 e2
| Eq (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Eq (e1',e2',s)) e1 e2
| App (e1,e2,s) ->
    mk_bin (fun e1' e2' -> App (e1',e2',s)) e1 e2
| Plus (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Plus (e1',e2',s)) e1 e2
| Minus (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Minus (e1',e2',s)) e1 e2
| Ratio (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Ratio (e1',e2',s)) e1 e2
| Mult (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Mult (e1',e2',s)) e1 e2
| Div (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Div (e1',e2',s)) e1 e2
| Rem (e1,e2,s) ->
    mk_bin (fun e1' e2' -> Rem (e1',e2',s)) e1 e2

| Not (e,s)    -> let e',f = defun_exp e in Not(e',s), f
| Uplus (e,s)  -> let e',f = defun_exp e in Uplus(e',s), f
| Uminus (e,s) -> let e',f = defun_exp e in Uminus(e',s), f

| If (cases,s) ->
    let app (g,e,s) =
      let e',f = defun_exp e in g, f e', s in
    let cases' = List.map app cases
    in If (cases',s), id

| Case (e,cases,s) ->
    let app (p,g,e) =
      let e',f = defun_exp e in p, g, f e' in
    let cases' = List.map app cases in
    let e',f = defun_exp e
    in Case (e',cases',s), f

| Try (e,cases,catches,s) ->
    let app1 (p,g,e) =
      let e',f = defun_exp e in p, g, f e' in
    let cases' = List.map app1 cases in
    let app2 (p,e) =
      let e',f = defun_exp e in p, f e' in
    let catches' = List.map app2 catches in
    let e',f = defun_exp e
    in Try (e',cases',catches',s), f

| Throw (e,s) -> let e',f = defun_exp e in Throw(e',s), f

| Ref (qname,arity,_) ->
    Eatom (Atom (reg_ref (qname,arity)), Error.dummy), id

| Lambda (clauses, _) -> reg_lam (defun_clauses clauses), id

| Call (e,es,s) ->
    let app e (prev,bind) =
      let e',f = defun_exp e in e' :: prev, compose f bind in
    let es', f_arg = List.fold_right app es ([],id)
    in begin match e with
          QName (Eatom (Atom _,_),Eatom (Atom _,_),_)
        | Eatom (Atom _,_) ->  Call (e,es',s), f_arg
        | _ -> let d = Error.dummy in
               let e',f = defun_exp e in
               let v = Var (gen_var ()), d in
               let two = Call (Eatom (Atom "element",d),
                               [Eatom (Nat 2,d); e'],d) in
               let args = [two; Elist (es',None)] in 
               let one = Call (Eatom (Atom "element",d),
                               [Eatom (Nat 1,d); e'],d) in
               let qname = QName (one, Eatom (Atom "app", d), d) in
               let lin e =
                 LetIn (Patom v, Call (qname, args, s), e, d)
               in Eatom v, compose (compose f f_arg) lin
       end

| Eatom _ as e -> e, id

| Elist (es, None) ->
    let app e (prev,bind) = 
      let e',f = defun_exp e in e'::prev, compose f bind in
    let es',f = List.fold_right app es ([],id)
    in Elist (es',None), f

| Elist (es, Some e) ->
    let e',f1 = defun_exp e in
    let app e (prev,bind) = 
      let e',f = defun_exp e in e'::prev, compose f bind in
    let es',f2 = List.fold_right app es ([],id)
    in Elist (es', Some e'), compose f1 f2

| Etuple es ->
    let app e (prev,bind) = 
      let e',f = defun_exp e in e'::prev, compose f bind in
    let es',f = List.fold_right app es ([],id)
    in Etuple es', f

| QName (e1,e2,s) -> 
  let e1',f1 = defun_exp e1 and e2',f2 = defun_exp e2
  in QName (e1',e2',s), compose f1 f2

let defun (name,arity,clauses) : Ast.fun_def =
  name, arity, defun_clauses clauses

let apply (out_mod: string) ((_,s,fun_defs): Ast.t) : Ast.t =
  out_module := out_mod;
  let fun_defs' = List.map defun fun_defs
in out_mod, s, ("app",2,get_clauses())::fun_defs'
