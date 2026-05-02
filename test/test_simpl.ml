open OUnit2
open Simpl.Ast
open Simpl.Eval
open Simpl.Parse

let eval_expr e = eval empty_env e
let eval_str s = eval empty_env (parse s)

let t name expected expr =
  name >:: fun _ -> assert_equal expected (eval_expr expr)

let t_ok name expected expr = t name (Ok expected) expr
let t_err name (expected : string) expr = t name (Error expected) expr

(* parser test utils *)
let tp name expected s = name >:: fun _ -> assert_equal expected (eval_str s)

let ast_tests =
  "ast"
  >::: [
         t_ok "int literal" (VInt 42) (Int 42);
         t_ok "bool literal" (VBool true) (Bool true);
         t_ok "add" (VInt 5) (Binop (Add, Int 2, Int 3));
         t_ok "sub" (VInt 1) (Binop (Sub, Int 3, Int 2));
         t_ok "mul" (VInt 6) (Binop (Mul, Int 2, Int 3));
         t_ok "eq true" (VBool true) (Binop (Eq, Int 1, Int 1));
         t_ok "eq false" (VBool false) (Binop (Eq, Int 1, Int 2));
         t_ok "lt true" (VBool true) (Binop (Lt, Int 1, Int 2));
         t_ok "if true" (VInt 1) (If (Bool true, Int 1, Int 2));
         t_ok "if false" (VInt 2) (If (Bool false, Int 1, Int 2));
         t_ok "let" (VInt 3) (Let ("x", Int 3, Var "x"));
         t_ok "let shadow" (VInt 2)
           (Let ("x", Int 1, Let ("x", Int 2, Var "x")));
         t_ok "fun is value"
           (VFun ("x", Var "x", empty_env))
           (Fun ("x", Var "x"));
         t_ok "app identity" (VInt 5) (App (Fun ("x", Var "x"), Int 5));
         t_ok "app add" (VInt 7)
           (App (Fun ("x", Binop (Add, Var "x", Int 2)), Int 5));
         t_ok "closure captures env" (VInt 3)
           (Let
              ("x", Int 1, App (Fun ("y", Binop (Add, Var "x", Var "y")), Int 2)));
         t_err "unbound var" "unbound var" (Var "x");
         t_err "type error in binop" "Operator and operand type mismatch"
           (Binop (Add, Int 1, Bool true));
         t_err "if guard not bool" "Guard of if must have type bool"
           (If (Int 1, Int 2, Int 3));
       ]

let parse_tests =
  "parse"
  >::: [
         tp "int" (Ok (VInt 1)) "1";
         tp "add" (Ok (VInt 3)) "1 + 2";
         tp "let" (Ok (VInt 5)) "let x = 5 in x";
         tp "if" (Ok (VInt 1)) "if true then 1 else 2";
         tp "fun app" (Ok (VInt 3)) "(fun x -> x + 1) 2";
         tp "nested let" (Ok (VInt 3)) "let x = 1 in let y = 2 in x + y";
         tp "closure" (Ok (VInt 5)) "let x = 3 in (fun y -> x + y) 2";
       ]

let () = run_test_tt_main ("simpl" >::: [ ast_tests; parse_tests ])
