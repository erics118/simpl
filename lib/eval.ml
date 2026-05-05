open Ast

let ( let* ) = Result.bind

module Env = Map.Make (String)

let empty_env = Env.empty

type env = value Env.t

and value =
  | VInt of int
  | VBool of bool
  | VFun of string * expr * env  (** function name, body, closure env *)
  | VPair of value * value
  | VLeft of value
  | VRight of value

(** [eval e] is the [e ==> v] relation. *)
let rec eval env expr =
  match expr with
  | Int v -> Ok (VInt v)
  | Bool v -> Ok (VBool v)
  | Fun (f, t, e) -> Ok (VFun (f, e, env))
  | Var x -> eval_var env x
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | Let (x, t, e1, e2) -> eval_let env x t e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | App (e1, e2) -> eval_app env e1 e2
  | Pair (e1, e2) ->
      let* v1 = eval env e1 in
      let* v2 = eval env e2 in
      Ok (VPair (v1, v2))
  | Fst e -> begin
      let* v = eval env e in
      match v with
      | VPair (v1, _) -> Ok v1
      | _ -> failwith "bad"
    end
  | Snd e -> begin
      let* v = eval env e in
      match v with
      | VPair (_, v2) -> Ok v2
      | _ -> failwith "bad"
    end
  | Left e ->
      let* v = eval env e in
      Ok (VLeft v)
  | Right e ->
      let* v = eval env e in
      Ok (VRight v)
  | Match (e, x1, e1, x2, e2) -> begin
      let* v = eval env e in
      match v with
      | VLeft v1 ->
          let env = Env.add x1 v1 env in
          eval env e1
      | VRight v2 ->
          let env = Env.add x2 v2 env in
          eval env e2
      | _ -> Error "match expects Left or Right"
    end

and eval_var env x =
  try Ok (Env.find x env) with Not_found -> Error "unbound var"

and eval_let env x t e1 e2 =
  let* v1 = eval env e1 in
  let env = Env.add x v1 env in
  eval env e2

and eval_if env e1 e2 e3 =
  let* v1 = eval env e1 in
  match v1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> Error "Guard of if must have type bool"

and eval_app env e1 e2 =
  begin
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    match v1 with
    | VFun (y, body, closure_env) -> eval (Env.add y v2 closure_env) body
    | _ -> failwith "lhs of app must be function"
  end

and eval_bop env bop e1 e2 =
  let* e1 = eval env e1 in
  let* e2 = eval env e2 in
  match (bop, e1, e2) with
  (* arithmetic *)
  | Add, VInt a, VInt b -> Ok (VInt (a + b))
  | Sub, VInt a, VInt b -> Ok (VInt (a - b))
  | Mul, VInt a, VInt b -> Ok (VInt (a * b))
  (* int equality *)
  | Eq, VInt a, VInt b -> Ok (VBool (a = b))
  | Neq, VInt a, VInt b -> Ok (VBool (a <> b))
  (* int comparison *)
  | Lt, VInt a, VInt b -> Ok (VBool (a < b))
  | Leq, VInt a, VInt b -> Ok (VBool (a <= b))
  | Gt, VInt a, VInt b -> Ok (VBool (a > b))
  | Geq, VInt a, VInt b -> Ok (VBool (a >= b))
  (* equality of bools *)
  | Eq, VBool a, VBool b -> Ok (VBool (a = b))
  | Neq, VBool a, VBool b -> Ok (VBool (a <> b))
  | _ -> Error "Operator and operand type mismatch"
