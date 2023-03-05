(* The constant [kwd] is the dictionary mapping the concrete syntax of
   the DSL keywords to their abstract syntax: the former are entries
   in a dictionary [Dict.t], and the latter are the associated
   definitions. The dictionary is implemented as a ternary search tree
   whose height has been minimised by balancing the subtrees as much
   as as possible. *)

val kwd : Parser.token Dict.t

(* Pretty-printing *)

val string_of_token : Parser.token -> string
