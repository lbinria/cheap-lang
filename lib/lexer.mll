{ 
  (* OCaml header, e.g. open Parser for token definitions *)
  open Parser
}

rule token = parse
  (* | [' ' '\t' '\n'] { token lexbuf }  skip whitespace *)
  | [' ' '\t'] { token lexbuf }  (* skip whitespace *)
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '\n'          { NEWLINE }
  | eof           { EOF }
  | _             { failwith "Unexpected character" }