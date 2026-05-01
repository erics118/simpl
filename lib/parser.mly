%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token <bool> BOOL
%token LET IN
%token EQ NEQ LT LEQ GT GEQ
%token PLUS MINUS TIMES
%token IF THEN ELSE
%token LPAREN RPAREN
%token EOF

%nonassoc IN ELSE
%nonassoc EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | IF cond = expr THEN e1 = expr ELSE e2 = expr    { If (cond, e1, e2) }
  | LET x = ID EQ e1 = expr IN e2 = expr            { Let (x, e1, e2) }
  | e1 = expr EQ e2 = expr                          { Binop (Eq, e1, e2) }
  | e1 = expr NEQ e2 = expr                         { Binop (Neq, e1, e2) }
  | e1 = expr LT e2 = expr                          { Binop (Lt, e1, e2) }
  | e1 = expr LEQ e2 = expr                         { Binop (Leq, e1, e2) }
  | e1 = expr GT e2 = expr                          { Binop (Gt, e1, e2) }
  | e1 = expr GEQ e2 = expr                         { Binop (Geq, e1, e2) }
  | e1 = expr PLUS e2 = expr                        { Binop (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr                       { Binop (Sub, e1, e2) }
  | e1 = expr TIMES e2 = expr                       { Binop (Mul, e1, e2) }
  | n = INT                                         { Int n }
  | n = BOOL                                        { Bool n }
  | x = ID                                          { Var x }
  | LPAREN e = expr RPAREN                          { e }
