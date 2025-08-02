type var_name = string 
type register_num = int 

exception UnbindVariable of string
exception InvalidSprite of string
exception MalformedCondition of string
exception NotImplemented

type expr_list = 
  | Expr of expr 
  | ExprList of expr * expr_list

and expr =
  | Clear
  | Binding of variable_binding
  | Assignment of assignment
  | Draw of var_or_value * var_or_value * var_name
  | Conditional_expr of conditional_expr

and var_or_value =
  | Var of var_name
  | Val of int

and assignment =
  | RegAssignment of register_num * int
  | VarAssignment of var_name * int

and conditional_expr = 
  | Single_statement of bool_expr * expr
  | Multi_statement of bool_expr * expr_list

and bool_expr = 
  | Eq of var_or_value * var_or_value
  | Neq of var_or_value * var_or_value

and variable_binding = var_name * assignment




(* Target: chip hex language *)
(* http://devernay.free.fr/hacks/chip8/C8TECH10.HTM *)
type chip_expr_list = 
  | ChipExpr of chip_expr 
  | ChipExprList of chip_expr * chip_expr_list
and chip_expr = 
  | CLS (* 00E0 *)
  | RET (* 00EE *)
  | JP of int (* 1nnn *)
  | CALL of int (* 2nnn *)
  | SE_Vx_Byte of register_num * int (* 3xkk *)
  | SNE_Vx_Byte of register_num * int (* 4xkk *)
  | SE_Vx_Vy of register_num * register_num (* 5xy0 *)
  | LD_Vx_Byte of register_num * int (* 6xkk *)
  | ADD_Vx_Byte of register_num * int (* 7xkk *)
  | LD_Vx_Vy of  register_num * register_num (* 8xy0 *)
  | OR_Vx_Vy of  register_num * register_num (* 8xy1 *)
  | AND_Vx_Vy of register_num * register_num (* 8xy2 *)
  | XOR_Vx_Vy of register_num * register_num (* 8xy3 *)
  | ADD_Vx_Vy of register_num * register_num (* 8xy4 *)
  | SUB_Vx_Vy of register_num * register_num (* 8xy5 *)
  | SHR_Vx_Vy of register_num * register_num (* 8xy6 *)
  | SUBN_Vx_Vy of register_num * register_num (* 8xy7 *)
  | SHL_Vx_Vy of register_num * register_num (* 8xyE *)
  | SNE_Vx_Vy of register_num * register_num (* 9xkk *)
  | LD_I_addr of int (* Annn *)
  | JP_V0_addr of int (* Bnnn *)
  | RND_Vx_Byte of register_num * int (* Cxkk *)
  | DRW of register_num * register_num * int (* Dxyn *)
  | SKP_Vx of register_num (* Ex9E *)
  | SKNP_Vx of register_num (* ExA1 *)
  | LD_Vx_DT of register_num (* Fx07 *)
  | LD_Vx_K of register_num (* Fx0A *)
  | LD_DT_Vx of register_num (* Fx15 *)
  | LD_ST_Vx of register_num (* Fx18 *)
  | ADD_I_Vx of register_num (* Fx1E *)
  | LD_F_Vx of register_num (* Fx29 *)
  | LD_B_Vx of register_num (* Fx33 *)
  | LD_I_Vx of register_num (* Fx55 *)
  | LD_Vx_I of register_num (* Fx65 *)

type subroutine = {
  offset: int;
  length: int;
  instructions: chip_expr list;
}

type program_data = { 
  bindings: (var_name, var_or_value) Hashtbl.t;
  registers: int array;
  sprites_data: SpriteAst.sprites_data;
  subroutines: subroutine list ref;
}


let rec get_reg_of_var data var_name = 

  let val_or_reg_opt = Hashtbl.find_opt data.bindings var_name in 
  match val_or_reg_opt with 
  | Some (Val reg) -> reg
  | Some (Var var) -> get_reg_of_var data var
  | None -> raise (UnbindVariable ("Unbind variable " ^ var_name ^ "."))


let rec transform data = function 
  (* | ExprList (e, l) -> ChipExprList (transform_expr data e, transform data l) *)
  | ExprList (e, l) -> 
      (* Should decomposed to be sure of the order of execution *)
      let a = transform_expr data e in 
      let b = transform data l in 
      a @ b
  | Expr e -> transform_expr data e

and transform_expr data = function 
  | Clear -> (* Printf.printf "Clear !\n";*) [CLS]
  | Binding (var, a) -> 
      (* Printf.printf "Bindings: %s\n" var; *)
      (match a with
      | RegAssignment (reg, v) -> 
          Hashtbl.replace data.bindings var (Val reg);
          (* Printf.printf "Bind %s with V%i\n" var reg; *)
          Array.set data.registers reg v;
          [LD_Vx_Byte (reg, v)]

      | VarAssignment (var_ass, v) ->
          Hashtbl.replace data.bindings var (Var var_ass);
          (* Printf.printf "Bind %s with %s\n" var var_ass; *)
          (* Search reg from variable *)
          let reg = get_reg_of_var data var in 
          Array.set data.registers reg v;
          [LD_Vx_Byte (reg, v)]
      )
  
  | Draw (x_param, y_param, sprite_name) -> 
    (* Printf.printf "Draw !\n"; *)
    let x_reg = 
      match x_param with 
      | Val reg -> reg
      | Var var -> get_reg_of_var data var
    in 
    let y_reg = 
      match y_param with 
      | Val reg -> reg
      | Var var -> get_reg_of_var data var
    in 
    let sprite_data_opt = Hashtbl.find_opt data.sprites_data.sprites_data_tbl sprite_name in 
    (match sprite_data_opt with 
    | Some sprite_data ->
      (* Printf.printf "offset: %i" (sprite_data.offset); *)
      [LD_I_addr sprite_data.offset;
      DRW (x_reg, y_reg, sprite_data.size)]
    | None -> raise (InvalidSprite ("Sprite " ^ sprite_name ^ " not found.\n"));
    )

  | Conditional_expr expr -> transform_conditional_expr data expr

  | _ -> []

and transform_conditional_expr data = function 
  | Single_statement (bool_expr, expr) -> 
      let converted_exprs = transform_expr data expr in 
      let plop = 
        match (List.length converted_exprs) with 
        | 1 -> List.nth converted_exprs 0
        | _ -> 
          (* TODO compute offset *)
          let last_subroutine_opt = List.nth_opt !(data.subroutines) 0 in 

          let offset = 
            match last_subroutine_opt with 
            | Some last_subroutine -> last_subroutine.offset + last_subroutine.length
            | None -> 0
          in 

          Printf.printf "Subroutine offset: %i, length: %i\n" offset ((List.length converted_exprs) + 1);
          
          let subroutine = {
            offset = offset;
            length = (List.length converted_exprs) + 1 (* Add RET *);
            instructions = converted_exprs @ [RET];
          } in 

          data.subroutines := subroutine :: !(data.subroutines);
          (* let data = { data with subroutines = subroutine :: data.subroutines } in  *)
          CALL offset
      in 
      [transform_bool_expr data bool_expr; plop]
  | Multi_statement (bool_expr, expr_list) ->
      let converted_exprs = transform data expr_list in 
      let plop = 
          (* TODO compute offset *)
          let last_subroutine_opt = List.nth_opt !(data.subroutines) 0 in 

          let offset = 
            match last_subroutine_opt with 
            | Some last_subroutine -> last_subroutine.offset + last_subroutine.length
            | None -> 0
          in 

          Printf.printf "Subroutine offset: %i, length: %i\n" offset ((List.length converted_exprs) + 1);
          
          let subroutine = {
            offset = offset;
            length = (List.length converted_exprs) + 1 (* Add RET *);
            instructions = converted_exprs @ [RET];
          } in 

          data.subroutines := subroutine :: !(data.subroutines);
          (* let data = { data with subroutines = subroutine :: data.subroutines } in  *)
          CALL offset
      in 
      [transform_bool_expr data bool_expr; plop]

and transform_bool_expr data = function 
  | Eq (Var vx, Val v) -> SNE_Vx_Byte (get_reg_of_var data vx, v)
  | Eq (Var vx, Var vy) -> SNE_Vx_Vy (get_reg_of_var data vx, get_reg_of_var data vy)
  | Neq (Var vx, Val v) -> SE_Vx_Byte (get_reg_of_var data vx, v)
  | Neq (Var vx, Var vy) -> SE_Vx_Vy (get_reg_of_var data vx, get_reg_of_var data vy)
  | _ -> raise (MalformedCondition ("Cannot compare two values in condition."))

    
(* Compile chip AST (transform to hex string) *)
let rec compile program_offset data sprite_ast (* default = 512 *) l =  
    (* offset of sprites in bytes *)
    let sprites_offset = (List.length l) * 2 (* Each instruction is 2 bytes *) in
    (* Compile sprites *)
    let sprites_bytes_str = SpriteAst.get_bytes sprite_ast in 
    (* compute subroutines offset in bytes *)
    let sprites_len = (String.length sprites_bytes_str) / 2 in
    let subroutines_offset = sprites_offset + sprites_len in
    
    let compile_subroutine subroutine = String.concat "" (List.map (compile_expr program_offset sprites_offset subroutines_offset) subroutine.instructions) in 
    let subroutines_bytes_str = String.concat "" (List.map compile_subroutine !(data.subroutines)) in 

    String.concat "" (List.map (compile_expr program_offset sprites_offset subroutines_offset) l)
    ^ sprites_bytes_str
    ^ subroutines_bytes_str

and compile_expr program_offset sprites_offset subroutines_offset = function 
  | CLS -> "00E0"
  | RET -> Printf.printf "RETURN"; "00EE"
  | JP nnn -> "1" ^ Printf.sprintf "%03x" (nnn + program_offset)
  | CALL nnn -> "2" ^ Printf.sprintf "%03x" (nnn + subroutines_offset + program_offset)
  | SE_Vx_Byte (x, kk) -> "3" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | SNE_Vx_Byte (x, kk) -> "4" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | SE_Vx_Vy (x, y) -> "5" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "0"
  | LD_Vx_Byte (x, kk) -> "6" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | ADD_Vx_Byte (x, kk) -> "7" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | LD_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "0"
  | OR_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "1"
  | AND_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "2"
  | XOR_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "3"
  | ADD_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "4"
  | SUB_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "5"
  | SHR_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "6"
  | SUBN_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "7"
  | SHL_Vx_Vy (x, y) -> "8" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "E"
  | SNE_Vx_Vy (x, y) -> "9" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ "0"
  | LD_I_addr nnn -> "A" ^ Printf.sprintf "%03x" (nnn + sprites_offset + program_offset)
  | JP_V0_addr nnn -> "B" ^ Printf.sprintf "%03x" (nnn + program_offset)
  | RND_Vx_Byte (x, kk) -> "C" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | DRW (x, y, n) -> "D" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ Printf.sprintf "%x" n
  | SKP_Vx x -> "E" ^ Printf.sprintf "%x" x ^ "9E"
  | SKNP_Vx x -> "E" ^ Printf.sprintf "%x" x ^ "A1"
  | LD_Vx_DT x -> "F" ^ Printf.sprintf "%x" x ^ "07"
  | LD_Vx_K x -> "F" ^ Printf.sprintf "%x" x ^ "0A"
  | LD_DT_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "15" 
  | LD_ST_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "18" 
  | ADD_I_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "1E" 
  | LD_F_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "29" 
  | LD_B_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "33" 
  | LD_I_Vx x -> "F" ^ Printf.sprintf "%x" x ^ "55" 
  | LD_Vx_I x -> "F" ^ Printf.sprintf "%x" x ^ "65" 


  
  

