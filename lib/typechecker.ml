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

(* fresh type variables *)
let fresh_counter = ref 0

(* creates a fresh type variable *)
let fresh () =
  incr fresh_counter;
  TVar ("'t" ^ string_of_int (Char.code 'a' + !fresh_counter))

(* constraint is a pair of types we want equal *)
type constraints = (typ * typ) list

(* substitution map of type variable names to types *)
type subst = (string * typ) list

let ( let* ) = Result.bind

(* apply a substitution to a single type *)
let rec apply_subst (s : subst) (t : typ) : typ =
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TFunc (param_t, ret_t) ->
      let param_t = apply_subst s param_t in
      let ret_t = apply_subst s ret_t in
      TFunc (param_t, ret_t)
  | TPair (a, b) ->
      let a = apply_subst s a in
      let b = apply_subst s b in
      TPair (a, b)
  | TVar x ->
      (* apply substitutions for each elem in list *)
      begin try apply_subst s (List.assoc x s) with Not_found -> TVar x
      end

(* apply a substitution to every constraint *)
let apply_subst_constraints (s : subst) (cs : constraints) : constraints =
  (* apply subst to and rhs of each constraint *)
  List.map (fun (a, b) -> (apply_subst s a, apply_subst s b)) cs

(* occurs check: does TVar x appear inside t? *)
let rec occurs (x : string) (t : typ) : bool =
  match t with
  | TInt -> false
  | TBool -> false
  | TFunc (param_t, ret_t) -> occurs x param_t || occurs x ret_t
  | TPair (a, b) -> occurs x a || occurs x b
  | TVar y -> x = y

(* unify a constraint list into a substitution *)
let rec unify (cs : constraints) : (subst, string) result =
  match cs with
  | [] -> Ok []
  | c :: cs ->
      begin match c with
      (* if l = r, just remove it *)
      | l, r when l = r -> unify cs
      (* pair *)
      (* one variable, subst it *)
      | TVar v, a | a, TVar v ->
          if occurs v a then Error "occurs check failed"
          else
            (* create the subst *)
            let s = [ (v, a) ] in
            (* apply the subst *)
            let cs = apply_subst_constraints s cs in
            (* recurse *)
            let* rest = unify cs in
            Ok ((v, a) :: rest)
      (* both functions, add two new constr *)
      | TFunc (p1, r1), TFunc (p2, r2) -> unify ((p1, p2) :: (r1, r2) :: cs)
      | TPair (a1, b1), TPair (a2, b2) -> unify ((a1, a2) :: (a2, b2) :: cs)
      | _ -> Error "cannot solve"
      end

let rec infer (env : StaticEnvironment.t) (e : expr) :
    (typ * constraints, string) result =
  match e with
  | Int _ -> Ok (TInt, [])
  | Bool _ -> Ok (TBool, [])
  | Var x ->
      let* t = StaticEnvironment.lookup env x in
      Ok (t, [])
  | Fun (x, t_opt, body) ->
      (* if val exists, use it *)
      let tx =
        match t_opt with
        | Some t -> t
        | None -> fresh ()
      in
      (* extend the environment to add x *)
      let env = StaticEnvironment.extend env x tx in
      let* tb, c = infer env body in
      Ok (TFunc (tx, tb), c)
  | App (e1, e2) ->
      let* t1, c1 = infer env e1 in
      let* t2, c2 = infer env e2 in
      let ret_t = fresh () in
      (* t1 must be the function of t2 -> ret_t *)
      Ok (ret_t, (t1, TFunc (t2, ret_t)) :: (c1 @ c2))
  | If (e1, e2, e3) ->
      let* t1, c1 = infer env e1 in
      let* t2, c2 = infer env e2 in
      let* t3, c3 = infer env e3 in
      Ok (t2, (t1, TBool) :: (t2, t3) :: (c1 @ c2 @ c3))
  | Binop (op, e1, e2) ->
      let* t1, c1 = infer env e1 in
      let* t2, c2 = infer env e2 in
      begin match op with
      (* ensure lhs, rhs both ints, returns int *)
      | Add | Sub | Mul -> Ok (TInt, (t1, TInt) :: (t2, TInt) :: (c1 @ c2))
      (* ensure lhs, rhs both ints, returns bool *)
      | Lt | Leq | Gt | Geq -> Ok (TBool, (t1, TInt) :: (t2, TInt) :: (c1 @ c2))
      (* ensure lhs, rhs same, returns bool *)
      | Eq | Neq -> Ok (TBool, (t1, t2) :: (c1 @ c2))
      end
  | Let (x, t_opt, e1, e2) ->
      let* t1, c1 = infer env e1 in
      (* if x doesn't have a type annotation, add the extra constraint *)
      let extra =
        match t_opt with
        | Some t -> [ (t1, t) ]
        | None -> []
      in
      let env = StaticEnvironment.extend env x t1 in
      (* infer e2 in the new environment *)
      let* t2, c2 = infer env e2 in
      Ok (t2, extra @ c1 @ c2)
  | Pair (a, b) ->
      let* ta, ca = infer env a in
      let* tb, cb = infer env b in
      Ok (TPair (ta, tb), ca @ cb)
  | Fst e ->
      let* t, c = infer env e in
      (* make fresh variables for lhs, rhs of pair *)
      let ta = fresh () and tb = fresh () in
      (* ensure t = (ta, tb) *)
      Ok (ta, (t, TPair (ta, tb)) :: c)
  | Snd e ->
      let* t, c = infer env e in
      (* make fresh variables for lhs, rhs of pair *)
      let ta = fresh () and tb = fresh () in
      (* ensure t = (ta, tb) *)
      Ok (tb, (t, TPair (ta, tb)) :: c)
  | Left _ | Right _ | Match _ -> Error "sum types currently not typechecked"

(** [typecheck e] typechecks [e] *)
let typecheck (e : expr) : (typ, string) result =
  (* reset fresh counter *)
  fresh_counter := 0;
  (* infer the only expr, e *)
  let* t, cs = infer StaticEnvironment.empty e in
  (* unify *)
  let* s = unify cs in
  (* apply the substitutions *)
  Ok (apply_subst s t)
