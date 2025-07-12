let () = 
  if Array.length Sys.argv <> 2 then begin 
    prerr_endline "";
    exit 1
  end;
  let filename = Sys.argv.(1) in 
  
  let in_chan = 
    try open_in filename
    with Sys_error msg -> 
      prerr_endline ("Error opening file: " ^ msg);
      exit 1
    in 

    let lexbuf = Lexing.from_channel in_chan in 

    let ast = SpriteParser.main SpriteLexer.token lexbuf in 
    Printf.printf "N sprites = %d\n" (SpriteAst.count ast);
    Printf.printf "N bytes = %d\n" (SpriteAst.get_size ast);
    Printf.printf "%s\n" (SpriteAst.get_bytes ast)
    (* let ast = 
      try 
        let ast = Parser.main Lexer.token lexbuf in 
        Printf.printf "Result = %d\n" (Ast.eval ast)
      with
      | Lexer.Error msg ->
        prerr_endline "bob";
        exit 1
      | Parser.Error msg ->
        prerr_endline "parse";
        exit 1
    in
    close_in in_chan; *)
     


(* let () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.main Lexer.token lexbuf in
  Printf.printf "Result = %d\n" (Ast.eval result) *)


(* let () = 
  if Array.length Sys.argv <> 2 then begin 
    prerr_endline "";
    exit 1
  end;
  let filename = Sys.argv.(1) in 
  
  let in_chan = 
    try open_in filename
    with Sys_error msg -> 
      prerr_endline ("Error opening file: " ^ msg);
      exit 1
    in 

    let lexbuf = Lexing.from_channel in_chan in 

    let ast = Parser.main Lexer.token lexbuf in 
    Printf.printf "Result = %d\n" (Ast.eval ast)
    (* let ast = 
      try 
        let ast = Parser.main Lexer.token lexbuf in 
        Printf.printf "Result = %d\n" (Ast.eval ast)
      with
      | Lexer.Error msg ->
        prerr_endline "bob";
        exit 1
      | Parser.Error msg ->
        prerr_endline "parse";
        exit 1
    in
    close_in in_chan; *)
     


(* let () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.main Lexer.token lexbuf in
  Printf.printf "Result = %d\n" (Ast.eval result) *) *)