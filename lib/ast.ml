type var_name = string 
type register_num = int 

exception UnbindVariable of string

exception InvalidSprite of string

type expr_list = 
  | Expr of expr 
  | ExprList of expr * expr_list

and expr =
  | Clear
  | Binding of variable_binding
  | Assignment of assignment
  | Draw of var_or_value * var_or_value * var_name

and var_or_value =
  | Var of var_name
  | Val of int

and assignment =
  | RegAssignment of register_num * int
  | VarAssignment of var_name * int

and variable_binding = var_name * assignment

type program_data = { 
  bindings : (var_name, var_or_value) Hashtbl.t;
  registers : int array;
  sprites_data : SpriteAst.sprites_data;
}

(* Target: chip hex language *)
(* http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#Annn *)
type chip_expr_list = 
  | ChipExpr of chip_expr 
  | ChipExprList of chip_expr * chip_expr_list
and chip_expr = 
  | CLS (* 00E0 *)
  | LD_Vx_Byte of register_num * int (* 3xkk *)
  | DRW of register_num * register_num * int (* Dxyn *)
  | LD_I_addr of int (* Annn *)
  | END

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
  | Expr e -> transform_expr data e @ [END]

and transform_expr data = function 
  | Clear -> Printf.printf "Clear !\n"; [CLS]
  | Binding (var, a) -> 
      Printf.printf "Bindings: %s\n" var;
      (match a with
      | RegAssignment (reg, v) -> 
          Hashtbl.replace data.bindings var (Val reg);
          Printf.printf "Bind %s with V%i\n" var reg;
          Array.set data.registers reg v;
          [LD_Vx_Byte (reg, v)]

      | VarAssignment (var_ass, v) ->
          Hashtbl.replace data.bindings var (Var var_ass);
          Printf.printf "Bind %s with %s\n" var var_ass;
          (* Search reg from variable *)
          let reg = get_reg_of_var data var in 
          Array.set data.registers reg v;
          [LD_Vx_Byte (reg, v)]
      )
  
  | Draw (x_param, y_param, sprite_name) -> 
    Printf.printf "Draw !\n";
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
      Printf.printf "offset: %i" (sprite_data.offset);
      [LD_I_addr (sprite_data.offset);
      (* [LD_I_addr (4096 - sprites_data.size + sprite_data.offset); *)
      DRW (x_reg, y_reg, sprite_data.size)]
    | None -> raise (InvalidSprite ("Sprite " ^ sprite_name ^ " not found.\n"));
    )

  | _ -> [END]


    
(* Compile chip AST (transform to hex string) *)
let rec compile l =  
    (* offset of sprites in bytes *)
    let sprites_offset = (List.length l) * 2 in
    String.concat "" (List.map (compile_expr sprites_offset) l)

and compile_expr sprites_offset = function 
  | CLS -> "00E0"
  | LD_Vx_Byte (x, kk) -> "6" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | DRW (x, y, n) -> "D" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ Printf.sprintf "%x" n
  | LD_I_addr nnn -> "A" ^ Printf.sprintf "%03x" (nnn + sprites_offset)
  | END -> "0000"