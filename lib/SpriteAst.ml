exception InvalidSprite of string


type sprites = 
  | Sprites of string * sprite * sprites
  | Sprite of string * sprite
  | NoSprites

and sprite = 
  | Rows of int * sprite
  | Row of int

type sprite_data = { 
  name : string;
  offset : int;
  size : int;
}

(* type sprites_data = (string, sprite_data) Hashtbl.t *)

type sprites_data = {
  size : int;
  sprites_data_tbl : (string, sprite_data) Hashtbl.t;
}



(* Count the number of sprites *)
let rec count = function 
  | Sprites (_, _, sprites) -> 1 + count sprites 
  | Sprite _ -> 1
  | NoSprites -> 0

(* Get the size of sprites in bytes *)
let rec get_size = function 
  | Sprites (_, s, sl) -> get_sprite_size s + get_size sl
  | Sprite (_, s) -> get_sprite_size s
  | NoSprites -> 0
(* Get the size of a sprite in bytes *)
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

let rec get_sprites_as_list = function 
  | Sprites (_, s, sl) -> s :: get_sprites_as_list sl
  | Sprite (_, s) -> [s]
  | NoSprites -> []

let to_sprites_data_list (* sl *) =
  let offset = 0 in  
  let rec to_sprites_data_list_rec off = function
  | Sprites (sprite_name, s, sl) -> 
      let sprite_size = get_sprite_size s in 
      { name = sprite_name; offset = off; size = sprite_size} :: (to_sprites_data_list_rec (off + sprite_size) sl)
  | Sprite (sprite_name, s) -> [{ name = sprite_name; offset = off; size = (get_sprite_size s)}]
  | NoSprites -> []
  in 
  to_sprites_data_list_rec offset (* sl *)

let list_to_hashtbl records =
  let table = Hashtbl.create (List.length records) in
  List.iter (fun record ->
    Hashtbl.add table record.name record
  ) records;
  table

let to_sprites_data expr = list_to_hashtbl (to_sprites_data_list expr)

(* Get offset of a sprite *)
(* let rec get_offset = *)

let rec get_sprite name = function
  | Sprites (sprite_name, s, sl) -> if sprite_name = name then s else get_sprite name sl
  | Sprite (sprite_name, s) -> 
      if sprite_name = name then s 
      else raise (InvalidSprite ("Sprite " ^ name ^ " doesn't exists."));
  | NoSprites -> raise (InvalidSprite ("Sprite " ^ name ^ " doesn't exists."));
