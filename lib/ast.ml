type bop =
  | Add
  | Sub
  | Mul
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
