{ 
  (* OCaml header, e.g. open Parser for token definitions *)
  open Parser
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip whitespace *)
  (* | [' ' '\t'] { token lexbuf }  skip whitespace *)
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  (* Keywords *)
  | "clear"       { CLEAR }
  | "sprites"     { SPRITES }
  | "0x"           { INT_BIT 0 }
  | "1x"           { INT_BIT 1 }
  | '\n'          { NEWLINE }

	(* Comparison operators *)
	| '='              { OP_EQ }
	(* Parentheses and the like *)
	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }
  (* Other *)
  | eof           { EOF }
  | _             { failwith "Unexpected character" }