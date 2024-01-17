open Core
include Expr
include Type
include Stmt
include Pat

(*
 TODO:
 - [ ] Scoped co-recursion is impossible in the current setup
 - [ ] use (type <expr>) in types to represent type of relations,
       useful for when we add module inference
 *)

let stmts_to_src show_a v = List.map v ~f:(stmt_to_src show_a) |> String.concat
