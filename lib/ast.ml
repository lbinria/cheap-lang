type root = 
  | Sprites of sprites * expr
  | Program of expr 
and sprites = 
  | NoSprites
  | Sprite of int

and expr =
  
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Clear

(* A simple evaluator for the AST *)
let rec eval_expr = function
  | Int n      -> n
  | Add (x,y)  -> eval_expr x + eval_expr y
  | Sub (x,y)  -> eval_expr x - eval_expr y
  | Clear -> 0
  
let eval_sprites = function 
  | NoSprites -> 0
  | Sprite i -> i

let eval = function 
  | Sprites (s, e) -> eval_sprites s + eval_expr e
  | Program e -> eval_expr e

(* Optional: pretty‐printer for debugging *)
(* let rec to_string_expr = function
  | Int n     -> string_of_int n
  | Add (x,y) -> "(" ^ to_string_expr x ^ " + " ^ to_string_expr y ^ ")"
  | Sub (x,y) -> "(" ^ to_string_expr x ^ " - " ^ to_string_expr y ^ ")"
  | Clear -> "Clear"

let to_string_sprites = function 
  | NoSprites -> "NoSprites"
  | Sprite i -> "Sprite"

let to_string = function
  | Sprites (s, e) -> to_string_sprites s ^ to_string_expr e
  | Program expr -> to_string_expr expr *)