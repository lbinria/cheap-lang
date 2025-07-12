%{ 
  open Ast
%}

%token <int> INT

%token <string> VAR_NAME 
%token <int> REGISTER
%token EOF NEWLINE
%token CLEAR
%token PLUS MINUS
%left PLUS MINUS
%token OP_EQ
%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON SEMICOLON

%start <Ast.expr_list> main
%%

main:
  | expr_list EOF { $1 }

expr_list:
  | expr NEWLINE expr_list { ExprList($1, $3) }
  | expr SEMICOLON expr_list { ExprList($1, $3) }
  | expr { Expr $1 }
  | expr SEMICOLON { Expr $1 }
  | expr SEMICOLON NEWLINE { Expr $1 }
  | expr NEWLINE { Expr $1 }

expr:
  | CLEAR { Clear }
  | variable_binding { VariableBinding $1 }

expr_end:
  | SEMICOLON {}
  | NEWLINE {}

variable_binding:
  | VAR_NAME COLON REGISTER { ($1, 0) }

assignment:
  | VAR_NAME OP_EQ {}
  | variable_binding OP_EQ {}