open Ast

module type StaticEnvironment = sig
  (** [t] is the type of a static environment. *)
  type t

  (** [empty] is the empty static environment. *)
  val empty : t

  (** [lookup env x] gets the binding of [x] in [env]. *)
  val lookup : t -> string -> (typ, string) result

  (** [extend env x ty] is [env] extended with a binding of [x] to [ty]. *)
  val extend : t -> string -> typ -> t
end

module StaticEnvironment : StaticEnvironment = struct
  type t = (string * typ) list

  let empty = []

  let lookup (env : t) x =
    try Ok (List.assoc x env) with Not_found -> Error "Unbound variable"

  let extend env x ty = (x, ty) :: env
end

(* open StaticEnvironment *)

let ( let* ) = Result.bind

let rec typeof (env : StaticEnvironment.t) : expr -> (typ, string) result =
  function
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | Var x -> StaticEnvironment.lookup env x
  | Fun (f, t, e) ->
      let env = StaticEnvironment.extend env f t in
      let* t_body = typeof env e in
      Ok (TFunc (t, t_body))
  | Binop (bop, e1, e2) -> begin
      let* t1 = typeof env e1 in
      let* t2 = typeof env e2 in
      match (bop, t1, t2) with
      (* arithmetic *)
      | (Add | Sub | Mul), TInt, TInt -> Ok TInt
      (* cmp between ints, or between bools *)
      | (Lt | Leq | Gt | Geq), TInt, TInt -> Ok TBool
      (* equality of ints returns bool *)
      | (Eq | Neq), TInt, TInt -> Ok TBool
      (* equality of bools returns bool *)
      | (Eq | Neq), TBool, TBool -> Ok TBool
      | _ -> Error "operator and operand type mismatch"
    end
  | Let (x, t, e1, e2) ->
      let* t1 = typeof env e1 in
      if t1 = t then
        let env = StaticEnvironment.extend env x t in
        typeof env e2
      else Error "invalid type annotation"
  | If (cond_e, then_e, else_e) ->
      let* else_t = typeof env cond_e in
      if else_t <> TBool then Error "condition must be bool"
      else
        let* t2 = typeof env then_e in
        let* t3 = typeof env else_e in
        if t2 = t3 then Ok t2
        else Error "then and else branch in if different types"
  | App (func, arg) -> begin
      let* t_func = typeof env func in
      let* t_arg = typeof env arg in
      match t_func with
      | TFunc (t_in, t_out) ->
          if t_arg = t_in then Ok t_out else Error "argument type mismatch"
      | _ -> Error "lhs is not function"
    end
  | Pair (le, re) ->
      let* lt = typeof env le in
      let* rt = typeof env re in
      Ok (TPair (lt, rt))
  | Fst e ->
      let* t = typeof env e in
      begin match t with
      | TPair (l, _) -> Ok l
      | _ -> Error "called fst on a non-pair"
      end
  | Snd e ->
      let* t = typeof env e in
      begin match t with
      | TPair (_, r) -> Ok r
      | _ -> Error "called snd on a non-pair"
      end
  | Left e -> Error "sum types currently not typechecked"
  | Right e -> Error "sum types currently not typechecked"
  | Match (e, x1, e1, x2, e2) -> Error "sum types currently not typechecked"

(** [typecheck e] typechshecks [e] *)
let typecheck (e : expr) : (typ, string) result =
  typeof StaticEnvironment.empty e
