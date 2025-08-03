{ 
  (* OCaml header, e.g. open Parser for token definitions *)
  open Parser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let register   = "V" ( ['0'-'9'] | "1"[ '0'-'6' ] )

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* Skip whitespace *)
  | "//" [^'\n']*     { token lexbuf } (* Skip C++-style comments: from '//' to end-of-line *)


  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  (* Keywords *)
  | "clear"       { CLEAR }
  | "draw"        { DRAW_FN }
  | "if"        { IF }
  | "sub"        { SUB }
  | "call"        { SUB_CALL }

  (* | register as n { REGISTER (int_of_string n) } *)
  | "V" ( ['0'-'9'] | "1"[ '0'-'6' ] as n)  { REGISTER (int_of_string n) }

  | (alpha) (alpha|digit|'_')* as s { VAR_NAME s }

	(* Comparison operators *)
	| "=="              { OP_EQ }
	| "!="              { OP_NEQ }
  | '='               { OP_ASS }
	(* Parentheses and the like *)
	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

  | ':'              { COLON }
  | ';'+              { SEMICOLON }
  | ','+              { COMMA }
  (* Other *)
  | eof           { EOF }
  | _             { failwith "Unexpected character" }