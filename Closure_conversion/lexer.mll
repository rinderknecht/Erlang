{
 (* Menhir specification for mini-Erlang *)

 (* Local errors *)

 exception Local_err of Error.message

 let lex_err msg buffer =
   raise (Error.Lexer (msg, Error.mk_seg buffer))

 let handle_error scan buffer =
   let s = Error.mk_seg buffer
 in try scan buffer with Local_err msg -> raise (Error.Lexer (msg,s))

 (* String processing *)

 let mk_str (len:int) (p:char list) : string =
   let s = String.make len ' ' in 
   let rec fill i =
     function [] -> s | c::l -> s.[i] <- c; fill (i-1) l
 in assert (len = List.length p); fill (len-1) p

(* Pretty-printing of tokens. See module [Parser]. *)

let string_of_token = 
  let open Parser in function
  AFTER       -> "after"
| ANDALSO     -> "andalso"
| CASE        -> "case"
| CATCH       -> "catch"
| DIV         -> "div"
| END         -> "end"
| ERROR       -> "error"
| FALSE       -> "false"
| FUN         -> "fun"
| IF          -> "if"
| NOT         -> "not"
| OF          -> "of"
| ORELSE      -> "orelse"
| REM         -> "rem"
| THROW       -> "throw"
| TRUE        -> "true"
| TRY         -> "try"
| WHEN        -> "when"
| COMMA       -> ","
| SEMI        -> ";"
| DOT         -> "."
| LPAR        -> "("
| RPAR        -> ")"
| LBRACKET    -> "["
| RBRACKET    -> "]"
| LBRACE      -> "{"
| RBRACE      -> "}"
| TIMES       -> "*"
| SLASH       -> "/"
| PLUS        -> "+"
| MINUS       -> "-"
| MID         -> "|"
| ARROW       -> "->"
| DEF         -> "="
| APPEND      -> "++"
| EQ          -> "=:="
| NEQ         -> "=/="
| LT          -> "<"
| LEQ         -> "=<"
| GT          -> ">"
| GEQ         -> ">="
| Wild        -> "_"
| Var s       -> "Var(" ^ s ^ ")"
| String s    -> "\"" ^ String.escaped s ^ "\""
| Atom s      -> "Atom(" ^ s ^ ")"
| QName (m,s) -> "QName(" ^ m ^ "," ^ s ^ ")"
| Nat n       -> "Nat(" ^ string_of_int n ^ ")"
| Float x     -> "Float(" ^ x ^ ")"

| Tdef        -> "::"

| CHAR        -> "char()"
| BOOLEAN     -> "boolean()"
| ANY_TYPE    -> "any()"
| STRING      -> "string()"
| LIST        -> "list"
| INTEGER     -> "integer()"
| FLOAT       -> "float()"

| MODULE      -> "-module"
| COMPILE     -> "-compile"
| TYPE        -> "-type"
| EXPORT_ALL  -> "export_all"

| BIND        -> ">>"
| DROP        -> ">>|"
| PAR         -> "||"

| ACCUMULATOR -> "accumulator"
| ANY         -> "any"
| BY          -> "by"
| CONTINUE    -> "continue"
| CONTROLLED  -> "controlled"
| DELAY       -> "delay"
| DISTRIBUTE  -> "distribute"
| DO          -> "do"
| EVERY       -> "every"
| EXECUTE     -> "execute"
| FOR         -> "for"
| INSPECT     -> "inspect"
| ITERATE     -> "iterate"
| NODE        -> "node"
| NODES       -> "nodes"
| ON          -> "on"
| ONE         -> "one"
| OR          -> "or"
| PROVIDING   -> "providing"
| RAISE       -> "raise"
| RETRY       -> "retry"
| RETURN      -> "return"
| SELECTED    -> "selected"
| STABLE      -> "stable"
| TASK        -> "task"
| WITH        -> "with"

| EOF         -> "EOF"

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
  newline    { Lexing.new_line lexbuf; token lexbuf }
| blank+     { token lexbuf }

| ","        { Parser.COMMA    }
| ";"        { Parser.SEMI     }
| "."        { Parser.DOT      }
| "("        { Parser.LPAR     }
| ")"        { Parser.RPAR     }
| "["        { Parser.LBRACKET }
| "]"        { Parser.RBRACKET }
| "{"        { Parser.LBRACE   }
| "}"        { Parser.RBRACE   }
| "*"        { Parser.TIMES    }
| "/"        { Parser.SLASH    }
| "+"        { Parser.PLUS     }
| "-"        { Parser.MINUS    }
| "|"        { Parser.MID      }
| "->"       { Parser.ARROW    }
| "="        { Parser.DEF      }
| "++"       { Parser.APPEND   }
| "=:="      { Parser.EQ       }
| "=/="      { Parser.NEQ      }
| "<"        { Parser.LT       }
| "=<"       { Parser.LEQ      }
| ">"        { Parser.GT       }
| ">="       { Parser.GEQ      }
| "_"        { Parser.Wild     }
| "::"       { Parser.Tdef     }

| "-module"    { Parser.MODULE     }
| "-compile"   { Parser.COMPILE    }
| "-type"      { Parser.TYPE       }
| "export_all" { Parser.EXPORT_ALL }

| "any()"      { Parser.ANY_TYPE   }
| "float()"    { Parser.FLOAT      }
| "integer()"  { Parser.INTEGER    }
| "char()"     { Parser.CHAR       }
| "string()"   { Parser.STRING     }
| "boolean()"  { Parser.BOOLEAN    }
| "list()"     { Parser.LIST       }

| "after"    { Parser.AFTER    }
| "andalso"  { Parser.ANDALSO  }
| "case"     { Parser.CASE     }
| "catch"    { Parser.CATCH    }
| "div"      { Parser.DIV      }
| "end"      { Parser.END      }
| "false"    { Parser.FALSE    }
| "fun"      { Parser.FUN      }
| "if"       { Parser.IF       }
| "not"      { Parser.NOT      }
| "of"       { Parser.OF       }
| "orelse"   { Parser.ORELSE   }
| "rem"      { Parser.REM      }
| "throw"    { Parser.THROW    }
| "true"     { Parser.TRUE     }
| "try"      { Parser.TRY      }
| "when"     { Parser.WHEN     }


| ">>"          { Parser.BIND }
| ">>|"         { Parser.DROP }
| "||"          { Parser.PAR  }

| "accumulator" { Parser.ACCUMULATOR }
| "after"       { Parser.AFTER       }
| "by"          { Parser.BY          }
| "continue"    { Parser.CONTINUE    }
| "controlled"  { Parser.CONTROLLED  }
| "delay"       { Parser.DELAY       }
| "distribute"  { Parser.DISTRIBUTE  }
| "do"          { Parser.DO          }
| "every"       { Parser.EVERY       }
| "execute"     { Parser.EXECUTE     }
| "for"         { Parser.FOR         }
| "inspect"     { Parser.INSPECT     }
| "iterate"     { Parser.ITERATE     }
| "node"        { Parser.NODE        }
| "nodes"       { Parser.NODES       }
| "on"          { Parser.ON          }
| "one"         { Parser.ONE         }
| "or"          { Parser.OR          }
| "providing"   { Parser.PROVIDING   }
| "raise"       { Parser.RAISE       }
| "retry"       { Parser.RETRY       }
| "return"      { Parser.RETURN      }
| "selected"    { Parser.SELECTED    }
| "stable"      { Parser.STABLE      }
| "task"        { Parser.TASK        }
| "with"        { Parser.WITH        }

| decimal as num {
    try Parser.Nat (int_of_string num) with
      Failure "int_of_string" ->
        lex_err "Integer too large." lexbuf }
| float as num
    { Parser.Float num }
| u_ident as id
    { Parser.Var id }
| l_ident as id
    { Parser.Atom id }
| (l_ident as m) ':' (l_ident as id)
    { Parser.QName (m,id) }
| "%" { handle_error in_comment lexbuf;
        token lexbuf }
| '"' { handle_error (in_string 0 []) lexbuf }
| eof { Parser.EOF }
| _   { let open Error
        in raise (Lexer ("Invalid character.", mk_seg lexbuf)) }

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
                     output_string cout (string_of_token t);
                     output_string cout "\n";
                     flush cout;
                     iter ()
                   end
          with Error.Lexer diag -> Error.print "Lexical" diag
        in iter ()
  with Sys_error msg -> prerr_endline msg
}
