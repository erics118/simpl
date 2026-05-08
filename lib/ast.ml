type typ =
  | TInt
  | TBool
  | TFunc of typ * typ (* input type, output type *)
  | TPair of typ * typ
  | TVar of string (* type variables *)

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
  | Binop of bop * expr * expr (* e1 bop e2 *)
  | Let of string * typ option * expr * expr  (** let x : t = e1 in e2 *)
  | If of expr * expr * expr (* if e1 then e2 else e3 *)
  | Fun of string * typ option * expr (* fun x : int -> e *)
  | App of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * string * expr * string * expr
