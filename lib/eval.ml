open Ast

type error = string

let ( let* ) = Result.bind

(** [subst e v x] is [e{v/x}]. no errors are possible, so no monads *)
let rec subst e v x =
  match e with
  | Var y ->
      (* if same, then use the value of x, otherwise use the value of y *)
      if x = y then v else e
  (* values, no substitution can be done *)
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) ->
      (* just substitute into both sides of bop *)
      Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
      let e1 = subst e1 v x in
      if x = y then
        (* x is shadowed by y, so x is not free in e2 *)
        Let (y, e1, e2)
      else
        (* no shadowing, so re-bind inside *)
        Let (y, e1, subst e2 v x)
  | If (e1, e2, e3) ->
      (* no shadowing, just substitute everywhere *)
      If (subst e1 v x, subst e2 v x, subst e3 v x)
  | Fun (y, e) ->
      if x = y then
        (* if shadowing *)
        Fun (y, e)
      else
        (* no shadowing, just substitute *)
        Fun (y, subst e v x)
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)

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
  | If (e1, e2, e3) -> begin
      let* e1 = eval e1 in
      match e1 with
      | Bool true -> eval e2
      | Bool false -> eval e3
      | _ -> Error "Guard of if must have type bool"
    end
  (* functions are values *)
  | Fun _ as v -> Ok v
  | App (e1, e2) -> begin
      let* e1 = eval e1 in
      let* e2 = eval e2 in
      match e1 with
      | Fun (y, body) -> eval (subst body e2 y)
      | _ -> failwith "lhs of app must be function"
    end
