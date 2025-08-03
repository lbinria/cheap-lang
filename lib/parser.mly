%{ 
  open Ast
%}

%token <int> INT

%token <string> VAR_NAME 
%token <int> REGISTER
%token EOF NEWLINE
%token CLEAR DRAW_FN IF SUB SUB_CALL
%token PLUS MINUS
%left PLUS MINUS
%token OP_ASS OP_EQ OP_NEQ
%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON SEMICOLON COMMA

%start <Ast.expr_list> main
%%

main:
  | expr_list EOF { $1 }

expr_list:
  | expr { Expr $1 }
  | expr expr_list { ExprList ($1, $2) }

expr:
  | CLEAR expr_terminator { Clear }
  | assignment expr_terminator { Assignment $1 }
  | binding expr_terminator { Binding $1 }
  | DRAW_FN LPAREN var_or_value COMMA var_or_value COMMA VAR_NAME RPAREN expr_terminator { Draw ($3, $5, $7) }
  | conditional_expr { Conditional_expr $1 }
  | SUB VAR_NAME LBRACE expr_list RBRACE { Subroutine ($2, $4) }
  | SUB_CALL VAR_NAME expr_terminator { SubCall $2 }

// expr_terminator_opt:
//   | expr_terminator {}
//   | {}

expr_terminator:
  | SEMICOLON expr_terminator {}
  // | NEWLINE expr_terminator {}
  | SEMICOLON {}
  // | NEWLINE {}

binding:
  | VAR_NAME COLON assignment { ($1, $3) }

assignment:
  | REGISTER OP_ASS INT { RegAssignment ($1, $3) }
  | VAR_NAME OP_ASS INT { VarAssignment ($1, $3) }

(* TODO a voir *)
parameter_binding:
  | var_or_value COLON assignment {}

conditional_expr:
  | IF LPAREN bool_expr RPAREN expr { Single_statement ($3, $5) }
  | IF LPAREN bool_expr RPAREN LBRACE expr RBRACE { Single_statement ($3, $6) }
  | IF LPAREN bool_expr RPAREN LBRACE expr_list RBRACE { Multi_statement ($3, $6) }

bool_expr:
  | var_or_value OP_EQ var_or_value { Eq ($1, $3) }
  | var_or_value OP_NEQ var_or_value { Neq ($1, $3) }

var_or_value:
  | VAR_NAME { Var $1 }
  | INT { Val $1 }