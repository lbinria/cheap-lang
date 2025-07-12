%{ 
  open SpriteAst
%}

%token <int> INT_BIT

%token EOF
%token <string> VAR_NAME
%token OP_EQ
%token LSQBRA RSQBRA
%start <SpriteAst.sprites> main
%%

main:
    | sprites EOF { $1 }

sprites:
    | VAR_NAME OP_EQ LSQBRA sprite RSQBRA sprites { Sprites ($1, $4, $6) }
    | VAR_NAME OP_EQ LSQBRA sprite RSQBRA { Sprite ($1, $4) }
    | { NoSprites }

sprite:
    | byte sprite { Rows ($1, $2)}
    | byte { Row $1 }

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