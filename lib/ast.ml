type bop =
  | Add
  | Sub
  | Mul

type expr =
  | Int of int
  | Var of string
  | Binop of bop * expr * expr
  | Let of string * expr * expr
