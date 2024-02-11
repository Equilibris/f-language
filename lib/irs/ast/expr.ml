open Core
open Pat

type ('a, 'expr) bind_expr = { name : 'a; value : 'expr; within : 'expr }
and 'expr call_expr = { callee : 'expr; arg : 'expr }
and ('a, 'expr) lambda_expr = { binding : 'a; content : 'expr }
and 'expr tuple_expr = 'expr list
and ('a, 'expr) constructor_expr = 'a * 'expr
and ('a, 'expr) pat_arm = 'a pat * 'expr

and ('a, 'expr) match_expr = {
  scrutinee : 'expr;
  arms : ('a, 'expr) pat_arm list;
}

and 'a expr =
  | Bind of ('a, 'a expr) bind_expr
  | Match of ('a, 'a expr) match_expr
  | Call of 'a expr call_expr
  | Lambda of ('a, 'a expr) lambda_expr
  | Tuple of 'a expr tuple_expr
  | Constructor of ('a, 'a expr) constructor_expr
  | Id of 'a
[@@deriving show, eq]

let rec expr_to_src show_a =
  let p2s = pat_to_src show_a in
  let open Printf in
  function
  | Bind { name; value; within } ->
      sprintf "(let %s = %s in %s)" (show_a name) (expr_to_src show_a value)
        (expr_to_src show_a within)
  | Match { scrutinee; arms } ->
      sprintf "(match %s with%s)"
        (expr_to_src show_a scrutinee)
        (List.map
           ~f:(fun (pat, e) ->
             sprintf " | %s -> %s" (p2s pat) (expr_to_src show_a e))
           arms
        |> String.concat)
  | Call { callee; arg } ->
      sprintf "(%s %s)" (expr_to_src show_a callee) (expr_to_src show_a arg)
  | Lambda { binding; content } ->
      sprintf "(\\%s %s)" (show_a binding) (expr_to_src show_a content)
  | Tuple v ->
      "("
      ^ (List.map ~f:(fun v -> expr_to_src show_a v ^ ", ") v |> String.concat)
      ^ ")"
  | Constructor (name, v) ->
      sprintf "(%s %s)" (show_a name) (expr_to_src show_a v)
  | Id v -> show_a v
