{ 
  (* OCaml header, e.g. open Parser for token definitions *)
  open SpriteParser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

rule token = parse
    | [' ' '\t' '\n'] { token lexbuf }  (* skip whitespace *)
    (* Skip C++-style comments: from '//' to end-of-line *)
    | "//" [^'\n']*     { token lexbuf }
    | (alpha) (alpha|digit|'_')* as s { VAR_NAME s }
    (* Keywords *)
    | "0"           { INT_BIT 0 }
    | "1"           { INT_BIT 1 }
    (* Comparison operators *)
    | '='              { OP_EQ }
    (* Parentheses and the like *)
    | '['              { LSQBRA }
    | ']'              { RSQBRA }
    (* Other *)
    | eof           { EOF }
    | _             { failwith "Unexpected character" }