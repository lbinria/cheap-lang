type expr =
  | Int   of int
  | Add   of expr * expr
  | Sub   of expr * expr

(* A simple evaluator for the AST *)
let rec eval = function
  | Int n      -> n
  | Add (x,y)  -> eval x + eval y
  | Sub (x,y)  -> eval x - eval y

(* Optional: pretty‐printer for debugging *)
let rec to_string = function
  | Int n     -> string_of_int n
  | Add (x,y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
  | Sub (x,y) -> "(" ^ to_string x ^ " - " ^ to_string y ^ ")"