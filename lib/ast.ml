type var_name = string 
type register_num = int 

exception UnbindVariable of string

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
  variables : (var_or_value, int) Hashtbl.t;
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
  | ExprList (e, l) -> ChipExprList (transform_expr data e, transform data l)
  | Expr e -> ChipExpr (transform_expr data e)

and transform_expr data = function 
  | Clear -> CLS
  | Binding (var, a) -> 

      (match a with
      | RegAssignment (reg, v) -> 
          Hashtbl.replace data.bindings var (Val reg);
          Array.set data.registers reg v;
          LD_Vx_Byte (reg, v)

      | VarAssignment (var, v) ->
          Hashtbl.replace data.bindings var (Var var);
          (* Search reg from variable *)
          let reg = get_reg_of_var data var in 
          Array.set data.registers reg v;
          LD_Vx_Byte (reg, v)
      )
  
  (* | Draw (x_param, y_param, sprite) -> END *)

  | _ -> END
      

(* Compile chip AST (transform to hex string) *)
let rec compile = function 
  | ChipExpr e -> compile_expr e
  | ChipExprList (e, l) -> compile_expr e ^ compile l

and compile_expr = function 
  | CLS -> "00E0"
  | LD_Vx_Byte (x, kk) -> "6" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | DRW (x, y, n) -> "D" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ Printf.sprintf "%x" n
  | LD_I_addr nnn -> "A" ^ Printf.sprintf "%03x" nnn
  | END -> ""