%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token <bool> BOOL
%token LET IN
%token EQ NEQ LT LEQ GT GEQ
%token PLUS MINUS TIMES
%token FUN ARROW
%token IF THEN ELSE
%token LPAREN RPAREN
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | FUN x = ID ARROW e = expr                          { Fun (x, e) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr         { If (e1, e2, e3) }
  | LET x = ID EQ e1 = expr IN e2 = expr               { Let (x, e1, e2) }
  | e = expr_cmp                                       { e }

expr_cmp:
  | e1 = expr_add EQ e2 = expr_add    { Binop (Eq, e1, e2) }
  | e1 = expr_add NEQ e2 = expr_add   { Binop (Neq, e1, e2) }
  | e1 = expr_add LT e2 = expr_add    { Binop (Lt, e1, e2) }
  | e1 = expr_add LEQ e2 = expr_add   { Binop (Leq, e1, e2) }
  | e1 = expr_add GT e2 = expr_add    { Binop (Gt, e1, e2) }
  | e1 = expr_add GEQ e2 = expr_add   { Binop (Geq, e1, e2) }
  | e = expr_add                      { e }

expr_add:
  | e1 = expr_add PLUS e2 = expr_mul  { Binop (Add, e1, e2) }
  | e1 = expr_add MINUS e2 = expr_mul { Binop (Sub, e1, e2) }
  | e = expr_mul                      { e }

expr_mul:
  | e1 = expr_mul TIMES e2 = expr_app { Binop (Mul, e1, e2) }
  | e = expr_app                      { e }

expr_app:
  | e1 = expr_app e2 = expr_atom      { App (e1, e2) }
  | e = expr_atom                     { e }

expr_atom:
  | n = INT                           { Int n }
  | b = BOOL                          { Bool b }
  | x = ID                            { Var x }
  | LPAREN e = expr RPAREN            { e }
