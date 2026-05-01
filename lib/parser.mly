%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token LET IN
%token EQ PLUS MINUS TIMES
%token LPAREN RPAREN
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | LET x = ID EQ e1 = expr IN e2 = expr { Let (x, e1, e2) }
  | e = expr_add                          { e }

expr_add:
  | e1 = expr_add PLUS e2 = expr_mul  { Binop (Add, e1, e2) }
  | e1 = expr_add MINUS e2 = expr_mul { Binop (Sub, e1, e2) }
  | e = expr_mul                      { e }

expr_mul:
  | e1 = expr_mul TIMES e2 = expr_atom { Binop (Mul, e1, e2) }
  | e = expr_atom                      { e }

expr_atom:
  | n = INT                  { Int n }
  | x = ID                   { Var x }
  | LPAREN e = expr RPAREN   { e }
