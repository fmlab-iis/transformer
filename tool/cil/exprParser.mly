%{
  open Cil

  exception Undefined of string
  exception Redefined of string
  exception InvalidStatement of string
  exception InvalidAnnotation of string

  let lnum = ref 1
%}

%token <int> NUM
%token <string> VAR
%token LPAREN RPAREN LSQUARE RSQUARE
%token ADD SUB MUL DIV MOD NOT LAND LOR EQ NE GT GE LT LE SHIFTL SHIFTR
%token EOF

%start expr

%type <(string, Cil.varinfo) Hash.t -> Cil.exp> expr

%%


expr:
  aexpr                     { fun vmap -> $1 vmap }
 | aexpr LAND expr          { fun vmap -> BinOp (LAnd, $1 vmap, $3 vmap, intType) }
 | aexpr LOR expr           { fun vmap -> BinOp (LOr, $1 vmap, $3 vmap, intType) }
;

aexpr:
  shift_expr                { fun vmap -> $1 vmap }
 | aexpr EQ shift_expr      { fun vmap -> BinOp (Eq, $1 vmap, $3 vmap, intType) }
 | aexpr NE shift_expr      { fun vmap -> BinOp (Ne, $1 vmap, $3 vmap, intType) }
 | aexpr LT shift_expr      { fun vmap -> BinOp (Lt, $1 vmap, $3 vmap, intType) }
 | aexpr LE shift_expr      { fun vmap -> BinOp (Le, $1 vmap, $3 vmap, intType) }
 | aexpr GT shift_expr      { fun vmap -> BinOp (Gt, $1 vmap, $3 vmap, intType) }
 | aexpr GE shift_expr      { fun vmap -> BinOp (Ge, $1 vmap, $3 vmap, intType) }
;

shift_expr:
  as_expr                        { fun vmap -> $1 vmap }
 | shift_expr SHIFTL as_expr     { fun vmap -> BinOp (Shiftlt, $1 vmap, $3 vmap, intType) }
 | shift_expr SHIFTR as_expr     { fun vmap -> BinOp (Shiftrt, $1 vmap, $3 vmap, intType) }
;

as_expr:
  mdm_expr                       { fun vmap -> $1 vmap }
 | as_expr ADD mdm_expr          { fun vmap -> BinOp (PlusA, $1 vmap, $3 vmap, intType) }
 | as_expr SUB mdm_expr          { fun vmap -> BinOp (MinusA, $1 vmap, $3 vmap, intType) }
;

mdm_expr:
  primary                        { fun vmap -> $1 vmap }
 | mdm_expr MUL primary          { fun vmap -> BinOp (Mult, $1 vmap, $3 vmap, intType) }
 | mdm_expr DIV primary          { fun vmap -> BinOp (Div, $1 vmap, $3 vmap, intType) }
 | mdm_expr MOD primary          { fun vmap -> BinOp (Mod, $1 vmap, $3 vmap, intType) }
;

primary:
  NUM                      { fun vmap -> integer $1 }
 | SUB primary             { fun vmap -> UnOp (Neg, $2 vmap, intType) }
 | VAR                     
     { fun vmap -> 
       try
         Lval (Var (Hash.find vmap $1), NoOffset)
       with Not_found ->
         raise (Undefined ("Failed to find the varinfo of variable " ^ $1 ^ "."))
     }
 | NOT primary             { fun vmap -> UnOp (LNot, $2 vmap, intType)}
 | LPAREN expr RPAREN      { fun vmap -> $2 vmap}
;
