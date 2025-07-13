type var_name = string 
type register_num = int 


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
  variables : (var_or_value, int) Hashtbl.t;
  sprite_ast : SpriteAst.sprites;
}

(* Target: chip hex language *)
(* http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#Annn *)
type chip_expr_list = 
  | ChipExpr of chip_expr 
  | ChipExprList of chip_expr * chip_expr_list
and chip_expr = 
  | CLS (* 00E0 *)
  | LD_Vx_Byte of int * int (* 3xkk *)
  | DRW of int * int * int (* Dxyn *)
  | LD_I_addr (* Annn *)
  | END

let rec transform data = function 
  | ExprList (e, l) -> ChipExprList (transform_expr data e, transform data l)
  | Expr e -> ChipExpr (transform_expr data e)
and transform_expr data = function 
  | Clear -> CLS
  | Binding (var, a) -> Hashtbl.replace data.bindings var (transform_assignment data a); END
  | _ -> END
and transform_assignment data = function 
    | RegAssignment (reg, v) -> Hashtbl.replace data.variables (Val reg) v; Val reg
    | VarAssignment (var, v) -> Hashtbl.replace data.variables (Var var) v; Var var

let rec compile = function 
  | ChipExpr e -> compile_expr e
  | ChipExprList (e, l) -> compile_expr e ^ compile l
and compile_expr = function 
  | CLS -> "00E0"
  | LD_Vx_Byte (x, kk) -> "6" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | DRW (x, y, n) -> "D" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ Printf.sprintf "%x" n
  | LD_I_addr -> ""
  | END -> ""