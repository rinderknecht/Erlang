type message = string
type start = Lexing.position
type stop = Lexing.position
type seg = start * stop

let dummy = Lexing.dummy_pos, Lexing.dummy_pos

let is_dummy s = (s = dummy)

let mk_seg buffer =
  Lexing.lexeme_start_p buffer, Lexing.lexeme_end_p buffer

exception Lexer of (message * seg)
exception Parser of (message * seg)

let print (kind: string) (msg, (start, stop)) =
  let open Lexing
in assert (msg <> "");
   prerr_endline
   ((if kind = "" then msg else kind) ^ " error at line "
    ^ string_of_int start.pos_lnum ^ ", char " 
    ^ string_of_int (start.pos_cnum - start.pos_bol) 
    ^ (if stop.pos_lnum = start.pos_lnum
       then "--" ^ string_of_int (stop.pos_cnum - stop.pos_bol)
       else " to line " ^ string_of_int stop.pos_lnum
            ^ ", char " ^ string_of_int (stop.pos_cnum - stop.pos_bol))
    ^ (if kind = "" then "." else ":\n" ^ msg))

let signal (m: message) (s: seg) = raise (Parser(m,s))
