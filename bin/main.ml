open Simpl

let () =
  let line = input_line stdin in
  print_endline (Ast.show_expr (Parse.parse line))
