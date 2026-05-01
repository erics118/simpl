open Ast

type error = string

let ( let* ) = Result.bind

(** [subst e v x] is [e{v/x}]. *)
let subst _ _ _ = failwith "See next section"

(** [eval_bop bop v1 v2] applies [bop] to values [v1] and [v2]. *)
let eval_bop bop e1 e2 =
  match (bop, e1, e2) with
  (* arithmetic *)
  | Add, Int a, Int b -> Ok (Int (a + b))
  | Sub, Int a, Int b -> Ok (Int (a - b))
  | Mul, Int a, Int b -> Ok (Int (a * b))
  (* int equality *)
  | Eq, Int a, Int b -> Ok (Bool (a = b))
  | Neq, Int a, Int b -> Ok (Bool (a <> b))
  (* int comparison *)
  | Lt, Int a, Int b -> Ok (Bool (a < b))
  | Leq, Int a, Int b -> Ok (Bool (a <= b))
  | Gt, Int a, Int b -> Ok (Bool (a > b))
  | Geq, Int a, Int b -> Ok (Bool (a >= b))
  (* equality of bools *)
  | Eq, Bool a, Bool b -> Ok (Bool (a = b))
  | Neq, Bool a, Bool b -> Ok (Bool (a <> b))
  | _ -> Error "Operator and operand type mismatch"

(** [eval e] is the [e ==> v] relation. *)
let rec eval : expr -> (expr, error) result = function
  | (Int _ | Bool _) as v -> Ok v
  | Var _ -> Error "Unbound variable"
  | Binop (bop, e1, e2) ->
      let* e1 = eval e1 in
      let* e2 = eval e2 in
      eval_bop bop e1 e2
  | Let (x, e1, e2) ->
      let* e1 = eval e1 in
      eval (subst e2 e1 x)
  | If (e1, e2, e3) -> (
      let* e1 = eval e1 in
      match e1 with
      | Bool true -> eval e2
      | Bool false -> eval e3
      | _ -> Error "Guard of if must have type bool")
