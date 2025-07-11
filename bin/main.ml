
let () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.main Lexer.token lexbuf in
  Printf.printf "Result = %d\n" (Ast.eval result)