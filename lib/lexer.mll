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