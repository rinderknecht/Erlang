(* Pretty-printing of tokens. See module [Parser]. *)

let string_of_token = 
  let open Parser in function
  AFTER    -> "after"
| ANDALSO  -> "andalso"
| BEGIN    -> "begin"
| CASE     -> "case"
| CATCH    -> "catch"
| DIV      -> "div"
| END      -> "end"
| FALSE    -> "false"
| FUN      -> "fun"
| IF       -> "if"
| NOT      -> "not"
| OF       -> "of"
| ORELSE   -> "orelse"
| REM      -> "rem"
| THROW    -> "throw"
| TRUE     -> "true"
| TRY      -> "try"
| WHEN     -> "when"
| COMMA    -> ","
| SEMI     -> ";"
| COLON    -> ":"
| DOT      -> "."
| LPAR     -> "("
| RPAR     -> ")"
| LBRACKET -> "["
| RBRACKET -> "]"
| LBRACE   -> "{"
| RBRACE   -> "}"
| TIMES    -> "*"
| SLASH    -> "/"
| PLUS     -> "+"
| MINUS    -> "-"
| MID      -> "|"
| ARROW    -> "->"
| DEF      -> "="
| APPEND   -> "++"
| EQ       -> "=:="
| NEQ      -> "=/="
| LT       -> "<"
| LEQ      -> "=<"
| GT       -> ">"
| GEQ      -> ">="
| Wild     -> "_"
| Var s    -> "Var(" ^ s ^ ")"
| String s -> "\"" ^ String.escaped s ^ "\""
| Atom s   -> "Atom(" ^ s ^ ")"
| Nat n    -> "Nat(" ^ string_of_int n ^ ")"
| Float x   -> "Float(" ^ x ^ ")"
| EOF      -> "EOF"

(* The type [path] represents a path in a ternary search tree of type
   ['a Dict.t], therefore it contains the contents of the visited
   nodes (characters and strings) and all the directions to reach them
   from the root. [Left(c,p)] is a path starting with a [Leg] node
   containing the character [c] and taking a left turn, followed by a
   subpath [p]. [Right(c,p)] turns right and [Middle(pre,p)] goes
   in-between and corresponds either to a [Leg] node with equality or
   a [Cmp] node. [Epath] denotes the empty path. *)

type path = Left   of char * path
          | Middle of string * path
          | Right  of char * path
          | Epath  

(* The following exceptions are raised in presence of an invalid
   ternary search tree, depending on the nature of the defect. *)

(* [Trans p] means that a [Leg] or [Cmp] node at the end of path [p]
   breaks the monotonicity of the ordering. *)

exception Trans of path

(* [Typo(u,v)] means that the token at a leaf, whose concrete syntax
   is [v], does not match the external path, whose string is [u]. *)

exception Typo of string * string

(* [Ecmp p] means that path [p] ends with an empty [Cmp] node. *)

exception Ecmp of path

(* [LegNil p] means that path [p] ends with a [Leg] node with an empty
tree [Nil] on the middle. *)

exception LegNilNil of path

(* The call [copy i d j s] copies the string [s] (source) into the
   string [d] (destination), from right to left, starting in [s] at
   index [j] and in [d] at index [i]. If either [i] or [j] is
   negative, no copy takes place. The value of the call is the
   rightmost unused index in [d]. *)

let rec copy (i:int) (d:string) (j:int) (s:string) : int =
  if i < 0 || j < 0 then i else (d.[i] <- s.[j]; copy (i-1) d (j-1) s)

(* The call [fill i d p] copies the contents of the path [p] in the
   string [d] (destination), starting at index [i]. Note that the type
   [path] records more information than we need here, so we discard
   the failed equality tests denoted by [Left] and [Right]. *)

let rec fill (i:int) (d:string) : path -> string = function
  Epath -> assert (i = -1); d
| Left(_,path) | Right(_,path) -> fill i d path
| Middle(pre,path) -> fill (copy i d (String.length pre - 1) pre) d path

(* The value of [distill n p] is a string made of the contents of path
   [p], whose length is [n]. *)

let distill len = assert (len >= 0); fill (len-1) (String.make len ' ')

(* The call [check test path len tree] checks the validity of the
   ternary search tree [tree]. The argument [test] is a function to be
   called on the root of [tree] to check its correct ordering with
   respect to its parent (if any). The argument [path] is the path in
   an embedding tree, down to [tree]: if there is none, the path is
   empty (see definition of type [path] above). The argument [len] is
   the length of the path, measured in number of characters.

   The first call is [check ok Epath 0 tree] because the path is
   empty, hence of length~0, and there is no ordering constraint on
   the root of [tree]. Note that [ok] always evaluate in [false] to
   mean that the order is correct.

   If the root of [tree] is a leaf [Def k], the token [k] is compared
   with the path [path], as they are expected to have equal textual
   representations. If not, exception [Typo] is raised (see definition
   above).

   If the root of [tree] is a prefix [Cmp(f,d)] and the string [f] is
   empty, the exception [Ecmp] is raised (see definition above). If
   not empty, its first character [f.[0]] must be in order, otherwise
   the exception [Trans] is raised (see definition above). If in
   order, the prefix is added to the current path, its length is added
   to the current length path, and a recursive call is performed to
   carry on.

   If the root of [tree] is a test [Leg(c,l,m,r)], the root [c] (that
   is, the character upon which the test is based) has to be in order,
   else the exception [Trans] is raised (see definition above). If in
   order, the left, middle and right subtrees are checked with
   recursive calls. The constraint on the right child is that it must
   be lower than or equal to the current root [c], that is, [(>=)
   c]. For the left child, the test is [(>=) c]. For the middle child,
   the test cannot fail because [c] is part of the leaves, so it must
   be stored in the path, whose length is incremented. *)

let chk_typo (k:Parser.token) (p:path) (n:int) = 
  let s,t = distill n p, string_of_token k
in if s <> t then raise (Typo(s,t))

let ok _ = false

let rec check : 
  type b.(char -> bool) -> path -> int -> ('a,b) Dict.tst -> unit =
  let open Dict in fun test p n -> function
    Nil              -> ()
  | Def k            -> chk_typo k p n
  | Cmp("",_)        -> raise (Ecmp p)
  | Cmp(s,t)         -> if test s.[0] then raise (Trans p);
                        check ok (Middle(s,p)) (n + String.length s) t
  | Leg(_,Nil,_,Nil) -> raise (LegNilNil p)
  | Leg(c,t1,t2,t3)  -> if test c then raise (Trans p);
                        check ((>=) c) (Right(c,p)) n t3;
                        check ok (Middle(String.make 1 c,p)) (n+1) t2;
                        check ((<=) c) (Left(c,p)) n t1
  | Pre(k,t)         -> chk_typo k p n; check test p n t

let verify (Dict.Make t) : unit = check ok Epath 0 t

(* See interface about [kwd]. *)

let kwd : Parser.token Dict.t =
  let open Parser in let open Dict
in Make (Leg('i',
           Leg('c',
             Leg('a',
               Nil,
               Leg('f',
                 Nil,
                 Cmp("ter",Def AFTER),
                 Cmp("ndalso",Def ANDALSO)),
               Cmp("begin",Def BEGIN)),
             Cmp("a",
               Leg('s',
                 Nil,
                 Cmp("e", Def CASE),
                 Cmp("tch", Def CATCH))),
             Leg('f',
               Leg('e',
                 Cmp("div",Def DIV),
                 Cmp("nd",Def END),
                 Nil),
               Leg('a',
                 Nil,
                 Cmp("lse",Def FALSE),
                 Cmp("un",Def FUN)),
               Nil)),
           Cmp("f",Def IF),
           Leg('t',
             Leg('o',
               Cmp("not",Def NOT),
               Leg('r',
                 Cmp("f",Def OF),
                 Cmp("else",Def ORELSE),
                 Nil),
               Cmp("rem",Def REM)),
             Leg('r',
               Cmp("hrow",Def THROW),
               Leg('u',
                 Nil,
                 Cmp("e",Def TRUE),
                 Cmp("y",Def TRY)),
               Nil),
             Cmp("when",Def WHEN))))

(* We check that [kwd] is a well formed ternary search tree. *)

let () = assert (() = verify kwd)

(* (* Printing all tokens in the dictionary as a side-effect. *)
let () = 
  List.iter (fun k -> prerr_endline (string_of_token k))
            (Dict.def kwd)
*)
