let _ = match Array.length Sys.argv with
  2 -> (try
          let cin = open_in Sys.argv.(1) in
          let buffer = Lexing.from_channel cin
          in try Parser.main Lexer.token buffer with
               Lexer.Error diag -> close_in cin;
                                   Lexer.prerr "Lexical" diag
             | Parser.Error ->
                 close_in cin;
                 Lexer.prerr "Syntactical"
                              ("Parse error.",
                               Lexing.lexeme_start_p buffer,
                               Lexing.lexeme_end_p buffer)
        with Sys_error msg -> prerr_endline msg)
| _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
