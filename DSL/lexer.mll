{
 (* ocamllex specification for mini-Erlang *)

 (* Lexical errors *)

 type message = string 
 type start = Lexing.position
 type stop = Lexing.position

 exception Error of (message * start * stop)
 exception Local_err of message

 let lex_err msg buffer =
   raise (Error (msg, Lexing.lexeme_start_p buffer,
                      Lexing.lexeme_end_p buffer))

 let handle_error scan buffer =
   let start = Lexing.lexeme_start_p buffer in
   let stop = Lexing.lexeme_end_p buffer
 in try scan buffer with
      Local_err msg -> raise (Error (msg,start,stop))

 let prerr (kind: string) (msg, start, stop) =
   let open Lexing
 in prerr_endline (kind ^ " error at line "
    ^ string_of_int start.pos_lnum ^ ", char " 
    ^ string_of_int (start.pos_cnum - start.pos_bol) 
    ^ (if stop.pos_lnum = start.pos_lnum
       then "--" ^ string_of_int (stop.pos_cnum - stop.pos_bol)
       else " to line " ^ string_of_int stop.pos_lnum
            ^ ", char " ^ string_of_int (stop.pos_cnum - stop.pos_bol))
    ^ ":\n" ^ msg)

 (* String processing *)

  let mk_str (len:int) (p:char list) : string =
    let s = String.make len ' ' in 
    let rec fill i =
      function [] -> s | c::l -> s.[i] <- c; fill (i-1) l
  in assert (len = List.length p); fill (len-1) p

}

(* Auxiliary regular expressions *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let letter = lowercase | uppercase
let alphanum = letter | digit | '_'
let u_ident = uppercase alphanum*
let l_ident = lowercase alphanum*
let decimal = digit+
let float = decimal '.' decimal (['e' 'E'] ['+' '-']? decimal)?

(* Rules *)

rule token = parse
  newline { Lexing.new_line lexbuf; token lexbuf }
| blank+ { token lexbuf }
| ","    { Parser.COMMA }
| ";"    { Parser.SEMI }
| ":"    { Parser.COLON }
| "."    { Parser.DOT }
| "("    { Parser.LPAR }
| ")"    { Parser.RPAR }
| "["    { Parser.LBRACKET }
| "]"    { Parser.RBRACKET }
| "{"    { Parser.LBRACE }
| "}"    { Parser.RBRACE }
| "*"    { Parser.TIMES }
| "/"    { Parser.SLASH }
| "+"    { Parser.PLUS }
| "-"    { Parser.MINUS }
| "|"    { Parser.MID }
| "->"   { Parser.ARROW }
| "="    { Parser.DEF }
| "++"   { Parser.APPEND }
| "=:="  { Parser.EQ }
| "=/="  { Parser.NEQ }
| "<"    { Parser.LT }
| "=<"   { Parser.LEQ }
| ">"    { Parser.GT }
| ">="   { Parser.GEQ }
| "_"    { Parser.Wild }
| decimal as num {
    try Parser.Nat (int_of_string num) with
      Failure "int_of_string" ->
        lex_err "Integer too large." lexbuf }
| float as num
    { Parser.Float num }
| u_ident as id
    { Parser.Var id }
| l_ident as id {
    match Dict.find id Lexis.kwd with
      Some key -> key
    |     None -> Parser.Atom id }
| "%" { handle_error in_comment lexbuf;
        token lexbuf }
| '"' { handle_error (in_string 0 []) lexbuf }
| eof { Parser.EOF }
| _   { raise (Error ("Invalid character.",
                      Lexing.lexeme_start_p lexbuf,
                      Lexing.lexeme_end_p lexbuf)) }

and in_comment = parse
  newline { Lexing.new_line lexbuf }
| eof     { () }
| _       { in_comment lexbuf }

and in_string len acc = parse
  "\\\""  { in_string (len+1) ('"'::acc) lexbuf }
| '"'     { Parser.String (mk_str len acc) }
| eof     { raise (Local_err "Unterminated string.") } 
| _       { let chr = Lexing.lexeme_char lexbuf 0
            in in_string (len+1) (chr::acc) lexbuf }

{
(* Standalone lexer for debugging purposes *)

type filename = string

let trace (name: filename) =
  try
    match open_in name with
      cin ->
        let buffer = Lexing.from_channel cin in
        let cout = stdout in
        let rec iter () =
          try
            match token buffer with
              Parser.EOF -> close_in cin; close_out cout
            | t -> begin
                     output_string cout (Lexis.string_of_token t);
                     output_string cout "\n";
                     flush cout;
                     iter ()
                   end
          with Error diag -> prerr "Lexical" diag
        in iter ()
  with Sys_error msg -> prerr_endline msg
}
