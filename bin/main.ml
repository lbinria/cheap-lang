let () = 
  if Array.length Sys.argv < 2 then begin 
    prerr_endline "Error a cheap file is expected";
    exit 1
  end;

  let filename = Sys.argv.(1) in 
  Printf.printf "Open file: %s\n" filename;
  
  let in_chan = 
    try open_in filename
    with Sys_error msg -> 
      prerr_endline ("Error opening file: " ^ msg);
      exit 1
    in 

    let lexbuf = Lexing.from_channel in_chan in 
    let sprite_ast = SpriteParser.main SpriteLexer.token lexbuf in 

    Printf.printf "N sprites = %d\n" (SpriteAst.count sprite_ast);
    Printf.printf "N bytes = %d\n" (SpriteAst.get_size sprite_ast);
    Printf.printf "%s\n" (SpriteAst.get_bytes sprite_ast);
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
        exit 1 *)
    close_in in_chan; 

  (* let my_sprite = SpriteAst.get_sprite "top_right" sprite_ast in 
  Printf.printf "sprite top_right: %d\n" (SpriteAst.get_sprite_size my_sprite); *)

  (* let all_sprites = SpriteAst.get_sprites_as_list sprite_ast in 
  let str_sprite_sizes = 
    List.map (fun (sprite_name, sprite) -> "Sprite " ^ sprite_name ^ " has size of " ^ string_of_int (SpriteAst.get_sprite_size sprite)) 
    all_sprites 
  in 
  Printf.printf (String.concat "\n" str_sprite_sizes); *)

  (* Sprite data display for debug *)
  let sprites_data = SpriteAst.to_sprites_data_list sprite_ast in 

  Printf.printf "-- Sprite list --\n";
  List.iter (fun (sprite_data : SpriteAst.sprite_data) -> 
    Printf.printf "{name = %s, offset = %i, size = %i}\n" sprite_data.name sprite_data.offset sprite_data.size;
  ) sprites_data;

  if Array.length Sys.argv = 3 then begin 
    let filename = Sys.argv.(2) in 
    Printf.printf "Open file: %s\n" filename;
    let in_chan = 
      try open_in filename 
      with Sys_error _ -> 
        prerr_endline ("Error opening file: " ^ filename);
        exit 1
      in 
      let lexbuf = Lexing.from_channel in_chan in 
      let ast = Parser.main Lexer.token lexbuf in 

      (* Program state and data *)
      let data : Ast.program_data = {
        bindings = Hashtbl.create 0; 
        variables = Hashtbl.create 0; 
        sprite_ast = sprite_ast
      } in 

      let chip_ast = Ast.transform data ast in 
      let hex = Ast.compile chip_ast in 
      Printf.printf "%s\n" hex;

      Printf.printf("End.");
  end;
     

