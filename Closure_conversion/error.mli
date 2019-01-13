type message = string
type start = Lexing.position
type stop = Lexing.position
type seg = start * stop

val dummy : seg
val is_dummy : seg -> bool

val mk_seg : Lexing.lexbuf -> seg

exception Lexer of (message * seg)
exception Parser of (message * seg)

val print : string ->  message * seg -> unit
val signal : message -> seg -> 'a
