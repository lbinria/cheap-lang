%{ 
  open Ast
%}

%token <int> INT

%token <string> VAR_NAME 
%token <int> REGISTER
%token EOF NEWLINE
%token CLEAR DRAW_FN
%token PLUS MINUS
%left PLUS MINUS
%token OP_EQ
%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON SEMICOLON COMMA

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
  | assignment { Assignment $1 }
  | binding { Binding $1 }
  | DRAW_FN LPAREN var_or_value COMMA var_or_value COMMA VAR_NAME RPAREN { Draw ($3, $5, $7) }

expr_end:
  | SEMICOLON {}
  | NEWLINE {}

binding:
  | VAR_NAME COLON REGISTER { ($1, $3) }

assignment:
  | VAR_NAME OP_EQ INT { Assignment ($1, $3) }
  | binding OP_EQ INT { BindingAssignment ($1, $3) }

var_or_value:
  | VAR_NAME { Var $1 }
  | INT { Val $1 }