{ 
  (* OCaml header, e.g. open Parser for token definitions *)
  open Parser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let register   = "v" ( ['0'-'9'] | "1"[ '0'-'6' ] )

rule token = parse
  | [' ' '\t'] { token lexbuf }  (* Skip whitespace *)
  | "//" [^'\n']*     { token lexbuf } (* Skip C++-style comments: from '//' to end-of-line *)


  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  (* Keywords *)
  | "clear"       { CLEAR }

  (* | register as n { REGISTER (int_of_string n) } *)
  | "v" ( ['0'-'9'] | "1"[ '0'-'6' ] as n)  { REGISTER (int_of_string n) }

  | (alpha) (alpha|digit|'_')* as s { VAR_NAME s }

	(* Comparison operators *)
	| '='              { OP_EQ }
	(* Parentheses and the like *)
	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

  | ':'              { COLON }
  | ';'+              { SEMICOLON }
  | '\n'+             { NEWLINE }
  (* Other *)
  | eof           { EOF }
  | _             { failwith "Unexpected character" }