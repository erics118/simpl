open Ast

type error = string

let ( let* ) = Result.bind

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

(** [subst e v x] is [e{v/x}]. *)
let subst _ _ _ = failwith "See next section"

(** [step] is the [-->] relation, that is, a single step of evaluation. *)
let rec step : expr -> (expr, error) result = function
  | Int _ | Bool _ -> Error "Does not step. Is value, so done"
  | Var _ -> Error "Unbound variable"
  (* if both are values, step the entire bop *)
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  (* if one is, other isnt, step *)
  | Binop (bop, e1, e2) when is_value e1 ->
      let* e2 = step e2 in
      Ok (Binop (bop, e1, e2))
  | Binop (bop, e1, e2) ->
      let* e1 = step e1 in
      Ok (Binop (bop, e1, e2))
  (* e1 must be a value, otherwise step for it *)
  | Let (x, e1, e2) when is_value e1 -> Ok (subst e2 e1 x)
  | Let (x, e1, e2) ->
      let* e1 = step e1 in
      Ok (Let (x, e1, e2))
  (* if the condition is fully evaluated, evaluate the correct arm *)
  | If (Bool true, e2, _) -> Ok e2
  | If (Bool false, _, e3) -> Ok e3
  (* basic type checking for if statement *)
  | If (Int _, _, _) -> Error "Guard of if must have type bool"
  (* if condition isnt evaluated, step it *)
  | If (e1, e2, e3) ->
      let* e1 = step e1 in
      Ok (If (e1, e2, e3))

(** [step_bop bop v1 v2] implements the primitive operation [v1 bop v2].
    Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 =
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
