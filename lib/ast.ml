type var_name = string 
type register_num = int 
type variable_binding = var_name * register_num

type expr_list = 
  | Expr of expr 
  | ExprList of expr * expr_list

and expr =
  | Clear
  | Assignment of assignment
  | Binding of variable_binding
  | Draw of var_or_value * var_or_value * var_name

and var_or_value =
  | Var of var_name
  | Val of int

and assignment =
  | BindingAssignment of variable_binding * int 
  | Assignment of var_name * int

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

let rec transform = function 
  | ExprList (e, l) -> ChipExprList (transform_expr e, transform l)
  | Expr e -> ChipExpr (transform_expr e)
and transform_expr = function 
  | Clear -> CLS
  | _ -> END

let rec compile = function 
  | ChipExpr e -> compile_expr e
  | ChipExprList (e, l) -> compile_expr e ^ compile l
and compile_expr = function 
  | CLS -> "00E0"
  | LD_Vx_Byte (x, kk) -> "6" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%02x" kk
  | DRW (x, y, n) -> "D" ^ Printf.sprintf "%x" x ^ Printf.sprintf "%x" y ^ Printf.sprintf "%x" n
  | LD_I_addr -> ""
  | END -> ""