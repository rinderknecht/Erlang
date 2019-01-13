let _ = match Array.length Sys.argv with
  2 -> (try
          let cin = open_in Sys.argv.(1) in
          let buffer = Lexing.from_channel cin
          in try let _ = Parser.main Lexer.token buffer in () with
               Error.Lexer diag -> close_in cin;
                                   Error.print "Lexical" diag
             | Error.Parser diag -> close_in cin;
                                    Error.print "Syntactical" diag
             | Parser.Error ->
                 close_in cin;
                 Error.print "" ("Parse",Error.mk_seg buffer)
        with Sys_error msg -> prerr_endline msg)
| _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
