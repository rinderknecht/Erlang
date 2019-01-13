let print_module (out:out_channel) (m:string) =
  output_string out ("-module(" ^ m ^ ").\n")

let print_compile_opt (out:out_channel) =
  output_string out ("-compile(export_all).\n")

let print_basic (out:out_channel) =
  let open Ast in function
  Var s | Float s | Atom s | String s
           -> output_string out s
| QName (m,s) -> output_string out (m ^ ":" ^ s);
| Nat n    -> output_string out (string_of_int n)
| True     -> output_string out "true"
| False    -> output_string out "false"
| Wild     -> output_char out '_'

let rec print_expr (out:out_channel) =
  let open Ast in function
  LetIn (p,e1,e2,_) -> print_pattern out p;
                       output_char out '=';
                       print_expr out e1;
                       output_string out ", ";
                       print_expr out e2

| OrElse (e1,e2,_) -> print_expr out e1;
                      output_string out " orelse ";
                      print_expr out e2
| AndAlso (e1,e2,_) ->
    (match e1 with
       OrElse (_,_,_) | Not (_,_) -> output_char out '(';
                                     print_expr out e1;
                                     output_char out ')'
     | _ -> print_expr out e1);
    output_string out " andalso ";
    (match e2 with
       OrElse (_,_,_) | Not (_,_) -> output_char out '(';
                                     print_expr out e2;
                                     output_char out ')'
     | _ -> print_expr out e2)
| Not (e,_) ->
    (match e with
       OrElse(_,_,_) | AndAlso(_,_,_) ->
         output_string out "not (";
         print_expr out e;
         output_char out ')'
     | _ -> output_string out "not "; print_expr out e)

| Leq (e1,e2,_) -> print_expr out e1; output_string out " =< ";
                   print_expr out e2
| Lt (e1,e2,_)  -> print_expr out e1; output_string out " < ";
                   print_expr out e2
| Geq (e1,e2,_) -> print_expr out e1; output_string out " >= ";
                   print_expr out e2
| Gt (e1,e2,_)  -> print_expr out e1; output_string out " > ";
                   print_expr out e2
| Neq (e1,e2,_) -> print_expr out e1; output_string out " =/= ";
                   print_expr out e2
| Eq (e1,e2,_)  -> print_expr out e1; output_string out " =:= ";
                   print_expr out e2

| App (e1,e2,_) -> print_expr out e1; output_string out " ++ ";
                   print_expr out e2

| Plus (e1,e2,_)  -> print_expr out e1; output_string out " + ";
                     print_expr out e2
| Minus (e1,e2,_) -> print_expr out e1; output_string out " - ";
                     print_expr out e2

| Ratio (e1,e2,_) ->
    (match e1 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e1;
                                     output_char out ')'
     | _ -> print_expr out e1);
    output_string out " / ";
    (match e2 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e2;
                                     output_char out ')'
     | _ -> print_expr out e2)

| Mult (e1,e2,_) -> 
    (match e1 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e1;
                                     output_char out ')'
     | _ -> print_expr out e1);
    output_string out " * ";
    (match e2 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e2;
                                     output_char out ')'
     | _ -> print_expr out e2)

| Div (e1,e2,_) ->
    (match e1 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e1;
                                     output_char out ')'
     | _ -> print_expr out e1);
    output_string out " div ";
    (match e2 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e2;
                                     output_char out ')'
     | _ -> print_expr out e2)

| Rem (e1,e2,_) ->
    (match e1 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e1;
                                     output_char out ')'
     | _ -> print_expr out e1);
    output_string out " rem ";
    (match e2 with
       Plus(_,_,_) | Minus(_,_,_) -> output_char out '(';
                                     print_expr out e2;
                                     output_char out ')'
     | _ -> print_expr out e2)

| Uplus (e,_) ->
   (match e with
      Call(_,_,_) | Eatom _ | Elist _ | Etuple _ ->
        output_string out "+("; print_expr out e;
        output_char out ')'
    | _ -> output_char out '+'; print_expr out e)

| Uminus (e,_) ->
   (match e with
      Call(_,_,_) | Eatom _ | Elist _ | Etuple _ ->
        output_string out "-("; print_expr out e;
        output_char out ')'
    | _ -> output_char out '-'; print_expr out e)

| If (ifs,_) -> output_string out "if"; print_if_clauses out ifs

| Case (e,cases,_) -> output_string out "case ";
                      print_expr out e;
                      output_string out " of\n";
                      print_case_clauses out cases;
                      output_string out "end"

| Try (e, [], catch, _) -> output_string out "try ";
                           print_expr out e;
                           output_string out "\ncatch\n";
                           print_catch_clauses out catch; 
                           output_string out "end"

| Try (e, trys, catch, _) -> output_string out "try ";
                             print_expr out e;
                             output_string out " of\n";
                             print_try_clauses out trys;
                             output_string out "catch\n";
                             print_catch_clauses out catch; 
                             output_string out "end"

| Throw (e,_) -> output_string out "throw ";
                 print_expr out e

| Ref ((mopt,s),a,_) -> 
    output_string out "fun ";
    (match mopt with
       Some m -> output_string out (m ^ ":")
     | None   -> ());
    output_string out s;
    output_string out "/";
    output_string out (string_of_int a)

| Lambda (clauses, _) -> output_string out "fun";
                         print_clauses out "" clauses;
                         output_string out " end"

| Call (e,exprs,_) ->
    (match e with
       Eatom(Atom a,_)      -> output_string out a
     | Eatom(QName (m,a),_) -> output_string out (m ^ ":" ^ a)
     |                    _ -> output_char out '(';
                               print_expr out e;
                               output_char out ')');
    output_char out '(';
    print_exprs out exprs;
    output_char out ')'

| Eatom (b, _) -> print_basic out b
| Elist (exprs, None) -> output_char out '[';
                         print_exprs out exprs;
                         output_char out ']'
| Elist (exprs, Some e) -> output_char out '[';
                           print_exprs out exprs;
                           output_char out '|';
                           print_expr out e;
                           output_char out ']'
| Etuple exprs -> output_char out '{';
                  print_exprs out exprs;
                  output_char out '}'

(* Variants for the general tasks *)

| Iter (e1, None,_) -> output_string out "iterate ";
                       print_expr out e1

| Iter (e1, Some e2, _) ->
    output_string out "iterate ";
    print_expr out e1;
    output_string out " with accumulator ";
    print_expr out e2

| Do (e1,e2,e3,_) -> output_string out "do ";
                     print_expr out e1;
                     output_string out " or ";
                     print_expr out e2;
                     output_string out " after ";
                     print_expr out e3

| Ret (e,_) -> output_string out "return ";
               print_expr out e

| Ctrl (e1,e2,None,_) -> print_expr out e1;
                         output_string out " controlled by ";
                         print_expr out e2

| Ctrl (e1,e2,Some e3,_) ->
    print_expr out e1;
    output_string out " controlled by ";
    print_expr out e2;
    output_string out " with accumulator ";
    print_expr out e3    

| Par (e1,e2,_) -> print_expr out e1;
                   output_string out " || ";
                   print_expr out e2

| Any (e1,e2,_) -> output_string out "any ";
                   print_expr out e1;
                   output_string out " of ";
                   print_expr out e2

| Dist (e,n,c,_) ->
    output_string out "distribute ";
    print_expr out e;
    (match n with
       One  -> output_string out " on one node"
     | Many -> output_string out " on nodes");
    print_constraint out c

| Exec (e,_) -> output_string out "execute ";
                print_expr out e

| Cont (e,_) -> output_string out "continue with ";
                print_expr out e

| Delay (e,_) -> output_string out "delay ";
                 print_expr out e

| Retry (e1,e2,_) -> output_string out "retry ";
                     print_expr out e1;
                     output_string out " every ";
                     print_expr out e2

| Raise (e,_) -> output_string out "raise (";
                 print_expr out e;
                 output_char out ')'

| Tref ((mopt,s),a,_) -> 
    output_string out "task ";
    (match mopt with
       Some m -> output_string out (m ^ ":")
     | None   -> ());
    output_string out s;
    output_string out "/";
    output_string out (string_of_int a)

| Tlam (clauses,_) -> output_string out "task";
                      print_clauses out "" clauses;
                      output_string out " end"

| Bind (e1,e2,_) -> print_expr out e1;
                    output_string out " >> ";
                    print_expr out e2

| Drop (e1,e2,_) -> print_expr out e1;
                    output_string out " >>| ";
                    print_expr out e2

| Insp (e,ins,_) -> output_string out "inspect ";
                    print_expr out e;
                    output_string out " for ";
                    print_inspect_clauses out ins;
                    output_string out " end"

and print_inspect_clauses (out:out_channel) = function
   [] -> assert false
| [c] -> print_inspect_clause out c; output_char out '\n'
| c::clauses ->
   print_inspect_clause out c; output_string out ";\n";
   print_inspect_clauses out clauses

and print_inspect_clause (out:out_channel)
                         (task_pattern, guard_opt, expr) =
  print_task_pattern out task_pattern;
  (match guard_opt with
           None -> ()
   | Some (e,_) -> output_string out " when ";
                   print_expr out e);
  output_string out " -> ";
  print_expr out expr

and print_task_pattern (out:out_channel) = function
  Stable pattern -> print_pattern out pattern
| Error pattern -> print_pattern out pattern

and print_constraint (out:out_channel) = function
  ([],_),_ -> assert false
| (exprs,e1_opt),e2_opt ->
    output_string out " providing [";
    print_exprs out exprs;
    (match e1_opt with
       None -> ()
     | Some e1 -> output_char out '|';
                  print_expr out e1);
    output_char out ']';
    (match e2_opt with
       None -> ()
     | Some e2 -> output_string out " selected by ";
                  print_expr out e2)

and print_try_clauses (out:out_channel) = function
       [] -> assert false
| clauses -> print_case_clauses out clauses

and print_catch_clauses (out:out_channel) = function
          [] -> ()
| c::clauses -> print_catch_clause out c;
                print_catch_clauses out clauses

and print_catch_clause (out:out_channel) (pattern,expr) =
  print_pattern out pattern;
  output_string out " -> ";
  print_expr out expr

and print_exprs (out:out_channel) = function
        [] -> ()
|      [e] -> print_expr out e
| e::exprs -> print_expr out e;
              output_string out ", ";
              print_exprs out exprs

and print_case_clause (out:out_channel) (pattern, guard_opt, expr) =
  print_pattern out pattern;
  (match guard_opt with
           None -> ()
   | Some (e,_) -> output_string out " when ";
                   print_expr out e);
  output_string out " -> ";
  print_expr out expr

and print_case_clauses (out:out_channel) = function
   [] -> assert false
| [c] -> print_case_clause out c; output_char out '\n'
| c::clauses ->
   print_case_clause out c; output_string out ";\n";
   print_case_clauses out clauses

and print_if_clauses (out:out_channel) = function
          [] -> assert false
|        [c] -> print_if_clause out c; output_string out ".\n"
| c::clauses -> print_if_clause out c; output_string out ";\n";
                print_if_clauses out clauses

and print_if_clause (out:out_channel) (expr1, expr2, _) =
  print_expr out expr1;
  output_string out " -> ";
  print_expr out expr2

and print_pattern (out:out_channel) = 
  let open Ast in function
  Patom (b,_)      -> print_basic out b
| Plist (patterns,None) -> output_char out '[';
                           print_plist out patterns;
                           output_char out ']'
| Plist (patterns,Some p) -> output_char out '[';
                             print_plist out patterns;
                             output_char out '|';
                             print_pattern out p;
                             output_char out ']'
| Ptuple patterns  -> output_char out '{';
                      print_plist out patterns;
                      output_char out '}'
| Palias (p1,p2,_) -> print_pattern out p1;
                      output_char out '=';
                      print_pattern out p2

and print_plist (out:out_channel) = function
       [] -> ()
|     [t] -> print_pattern out t
| t::more -> print_pattern out t;
             output_char out ',';
             print_plist out more

and print_patterns (out:out_channel) = function
           [] -> ()
|         [p] -> print_pattern out p
| p::patterns -> print_pattern out p;
                 output_string out ", ";
                 print_patterns out patterns

and print_clause (out:out_channel) (name:string)
    (patterns, guard_opt, expr) = 
begin
  output_string out name;
  output_char out '(';
  print_patterns out patterns;
  output_char out ')';
  (match guard_opt with
    None -> ()
   | Some (e,_) -> output_string out " when ";
                   print_expr out e);
  output_string out " -> ";
  print_expr out expr
end

and print_clauses (out:out_channel) (name:string) = function
   [] -> assert false
| [c] ->
    print_clause out name c;
    if name <> "" then output_string out ".\n";
    flush out
| c::clauses ->
    print_clause out name c; output_string out ";\n";
    print_clauses out name clauses

let print_kind (out: out_channel) : Ast.kind -> unit =
  let rec print : Ast.kind -> unit = function
    Task ((name, _, clauses), _) -> output_string out "task\n";
                                    print_clauses out name clauses
  | Func ((name, _, clauses), _) -> print_clauses out name clauses
in print

let print_tree (out:out_channel) (m,_,kinds: Ast.t) : unit =
  begin
    print_module out m;
    print_compile_opt out;
    List.iter (print_kind out) kinds
  end

let main (out:out_channel) (ast: Ast.t) : unit =
(*  print_tree out ast *)
  let ast',mods = Lift.up ast in
    let () = print_tree out ast' in
    let print ((m,_,_) as ast) =
      let cout = open_out (m ^ ".rea")
      in (print_tree cout ast; close_out cout)
  in List.iter print mods

let _ = match Array.length Sys.argv with
  3 ->
   (try
      let file_in = Sys.argv.(1) in
        if Filename.check_suffix file_in "rea"
        then
          let cin = open_in file_in in
            let buffer = Lexing.from_channel cin
          in try
               let t = Parser.main Lexer.token buffer in
                 let cout = open_out Sys.argv.(2)
               in close_in cin; main cout t; close_out cout
             with
               Error.Lexer diag ->
                 close_in cin; Error.print "Lexical" diag
             | Error.Parser diag ->
                 close_in cin; Error.print "Syntactical" diag
             | Parser.Error ->
                 close_in cin;
                 Error.print "" ("Parse",Error.mk_seg buffer)
        else prerr_endline "Error: Use the suffix .rea"; exit 1
    with Sys_error msg -> prerr_endline msg)
| _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " input.rea output")

(* Testing *)

(*
let main cout (m,_,kinds as t) =
  print_module cout m;
  print_compile_opt cout;
  List.iter (print_kind cout) kinds;
  (* Lift.print_fv stdout kinds; 
     output_string stdout "\n"; *)
  let env = Lift.mk_env t
  in (output_string stdout "Top-level environment:\n";
      Lift.print_env stdout env;
      output_string stdout "\n";
      flush stdout;
      (let env' = try Lift.close env with
                    Lift.Unbound ref ->
                      prerr_endline ("Unbound " ^
                        Lift.string_of_fref ref);
                      close_out stdout;
                      exit 1
       in output_string stdout "Closure:\n";
          Lift.print_env stdout env'));
  close_out stdout
*)

(*
let main cout (m,_,kinds as t) =
  print_tree cout t;
  let env = Lift.mk_env t
  in output_string stdout "Top-level environment:\n";
     Lift.print_env stdout env;
     output_string stdout "\n";
     flush stdout;
     close_out stdout
*)
