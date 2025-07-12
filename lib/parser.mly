%{ 
  open Ast
%}

%token <int> INT

%token PLUS MINUS EOF NEWLINE
%token CLEAR
%token OP_EQ
%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%left PLUS MINUS
%start <Ast.expr> main
%%

main:
  | expr EOF { $1 }

expr:
  | INT             { Int $1 }
  | expr PLUS expr  { Add ($1, $3) }
  | expr MINUS expr { Sub ($1, $3) }
  | CLEAR { Clear }