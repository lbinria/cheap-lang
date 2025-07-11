%{ 
  open Ast
%}

%token <int> INT
%token PLUS MINUS EOF NEWLINE
%left PLUS MINUS
%start <Ast.expr> main
%%

main:
  | expr EOF { $1 }
  | expr NEWLINE { $1 }

expr:
  | INT             { Int $1 }
  | expr PLUS expr  { Add ($1, $3) }
  | expr MINUS expr { Sub ($1, $3) }