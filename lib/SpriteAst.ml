type sprites = 
  | Sprites of string * sprite * sprites
  | Sprite of string * sprite
  | NoSprites

and sprite = 
  | Rows of int * sprite
  | Row of int


let rec count = function 
  | Sprites (_, _, sprites) -> 1 + count sprites 
  | Sprite _ -> 1
  | NoSprites -> 0

let rec get_size = function 
  | Sprites (_, s, sl) -> get_sprite_size s + get_size sl
  | Sprite (_, s) -> get_sprite_size s
  | NoSprites -> 0
and get_sprite_size = function 
  | Rows (_, s) -> 1 + get_sprite_size s 
  | Row _ -> 1

let rec get_bytes = function 
  | Sprites (_, s, sl) -> get_sprite_bytes s ^ get_bytes sl
  | Sprite (_, s) -> get_sprite_bytes s
  | NoSprites -> ""
and get_sprite_bytes = function 
  | Rows (i, r) -> (Printf.sprintf "%02x" i) ^ get_sprite_bytes r
  | Row i -> Printf.sprintf "%02x" i