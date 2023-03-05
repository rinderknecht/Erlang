type message = string

type start = Lexing.position
type stop = Lexing.position

exception Error of (message * start * stop)

(* Entry point *)

val token : Lexing.lexbuf -> Parser.token

(* Standalone lexer for debugging purposes. The string representations
   of the tokens are sent to standard output. *) 

type filename = string

val trace : filename -> unit

val prerr : string ->  message * Lexing.position * Lexing.position -> unit
