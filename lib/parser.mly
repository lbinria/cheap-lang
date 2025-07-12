%{ 
  open Ast
%}

%token <int> INT
%token <int> INT_BIT

%token PLUS MINUS EOF NEWLINE
%token CLEAR SPRITES
%token OP_EQ
%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%left PLUS MINUS
%start <Ast.root> main
%%

main:
  | root EOF { $1 }

root:
  | SPRITES OP_EQ LSQBRA sprites RSQBRA expr EOF { Sprites ($4, $6) }
  | expr EOF { Program $1 }

sprites:
  | { NoSprites }
  | LSQBRA INT_BIT RSQBRA { Sprite $2 }

byte:
  INT_BIT INT_BIT INT_BIT INT_BIT INT_BIT INT_BIT INT_BIT INT_BIT {
    $1 lsl 7
    |> fun acc -> acc + ($2 lsl 6)
    |> fun acc -> acc + ($3 lsl 5)
    |> fun acc -> acc + ($4 lsl 4)
    |> fun acc -> acc + ($5 lsl 3)
    |> fun acc -> acc + ($6 lsl 2)
    |> fun acc -> acc + ($7 lsl 1)
    |> fun acc -> acc + $8
  }

expr:
  | INT             { Int $1 }
  | expr PLUS expr  { Add ($1, $3) }
  | expr MINUS expr { Sub ($1, $3) }
  | CLEAR { Clear }