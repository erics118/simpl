open Simpl
open Ast
open Eval
open Typechecker
open Parse

let rec string_of_val : value -> string = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VPair (l, r) ->
      Printf.sprintf "(%s, %s)" (string_of_val l) (string_of_val r)
  | VFun (name, param, env) -> Printf.sprintf "fun %s" name
  | VLeft v -> string_of_val v
  | VRight v -> string_of_val v

let interp s : (unit, string) result =
  let parsed = parse s in
  let* _ = typecheck parsed in
  let* evaled = eval empty_env parsed in
  print_endline (string_of_val evaled);
  Ok ()
