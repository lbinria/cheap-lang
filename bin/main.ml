let hex_to_bytes hex_str =
  let len = String.length hex_str in
  if len mod 2 <> 0 then
    invalid_arg "hex_to_bytes: hex string must have an even length";
  
  let bytes = Bytes.create (len / 2) in
  let rec aux i =
    if i < len then (
      let byte_str = String.sub hex_str i 2 in
      let byte = int_of_string ("0x" ^ byte_str) in
      Bytes.set bytes (i / 2) (char_of_int byte);
      aux (i + 2)
    )
  in
  aux 0;
  bytes

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


  (* Sprite data display for debug *)
  let sprites_data_list = SpriteAst.to_sprites_data_list sprite_ast in 

  Printf.printf "-- Sprite list --\n";
  List.iter (fun (sprite_data : SpriteAst.sprite_data) -> 
    Printf.printf "{name = %s, offset = %i, size = %i}\n" sprite_data.name sprite_data.offset sprite_data.size;
  ) sprites_data_list;

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

      let sprites_data : SpriteAst.sprites_data = {
        size = SpriteAst.get_size sprite_ast;
        sprites_data_tbl = SpriteAst.list_to_hashtbl sprites_data_list;
      } in

      (* Program state and data *)
      let data : Ast.program_data = {
        bindings = Hashtbl.create 0; 
        registers = Array.make 15 0;
        sprites_data = sprites_data;
        subroutines = ref [];
      } in 

      Printf.printf "Go convert.\n";

      let chip_instructions_list = Ast.transform data ast in 
      let nb_instructions = List.length chip_instructions_list in 
      (* Add end program instruction, infinite loop on jump *)
      (* TODO do that in compile AST by adding union END *)
      let chip_instructions_list = chip_instructions_list @ [JP (512 + nb_instructions * 2)] in 

      (* Printf.printf "Nb instruction: %i\n" (nb_instructions + 1);
      Printf.printf "cheap AST converted to chip AST.\n"; *)
      
      (* Compile main program *)
      let hex = Ast.compile 512 data sprite_ast chip_instructions_list in 

      (* Compile subroutines *)
      (* let subroutines_bytes_str = List.map (Ast.compile 512) program_data.subroutines] *)

      (* let program_length = (String.length hex) / 2 in  *)
      (* Printf.printf "Program length: %i\n" program_length;
      Printf.printf "%s\n" hex; *)

      (* Printf.printf "Program bytes: %i\n" ((String.length hex) / 2);
      Printf.printf "Sprite bytes: %i\n" (String.length (SpriteAst.get_bytes sprite_ast) / 2); *)
      
      (* let repeat_char c n =
        let rec aux acc n =
          if n <= 0 then acc
          else aux (acc ^ String.make 1 c) (n - 1)
        in
        aux "" n
      in  *)
      
      (* let str_program = hex ^ (SpriteAst.get_bytes sprite_ast) in *)
      let str_program = hex in 

      (* Printf.printf "All program bytes: %i\n" ((String.length str_program) / 2); *)
      Printf.printf "%s\n" str_program;

      let byte_array = hex_to_bytes str_program in
      (* Printf.printf "Bytes: %s\n" (Bytes.to_string byte_array); *)

      let write_bytes_to_file filename bytes =
        let f = open_out filename in
        output f bytes 0 (Bytes.length bytes);
        close_out f
      in 
      write_bytes_to_file "out.ch8" byte_array;

      Printf.printf("End.");
  end;
     

