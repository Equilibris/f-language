open Core
open Pat

type 'a bind_expr = { name : 'a; value : 'a expr; within : 'a expr }
and 'a call_expr = { callee : 'a expr; arg : 'a expr }
and 'a lambda_expr = { binding : 'a; content : 'a expr }
and 'a tuple_expr = 'a expr list
and 'a constructor_expr = 'a * 'a expr
and lit_expr = int
and 'a pat_arm = 'a pat * 'a expr
and 'a match_expr = 'a expr * 'a pat_arm list

and 'a expr =
  | Bind of 'a bind_expr
  | Match of 'a match_expr
  | Call of 'a call_expr
  | Lambda of 'a lambda_expr
  | Tuple of 'a tuple_expr
  | Constructor of 'a constructor_expr
  | Id of 'a
  | Lit of lit_expr
[@@deriving show, eq]

let rec expr_to_src show_a =
  let e2s = expr_to_src show_a in
  let p2s = pat_to_src show_a in
  let open Printf in
  function
  | Bind { name; value; within } ->
      sprintf "(let %s = %s in %s)" (show_a name) (e2s value) (e2s within)
  | Match (value, arms) ->
      sprintf "(match %s with%s)" (e2s value)
        (List.map
           ~f:(fun (pat, e) -> sprintf " | %s -> %s" (p2s pat) (e2s e))
           arms
        |> String.concat)
  | Call { callee; arg } -> sprintf "(%s %s)" (e2s callee) (e2s arg)
  | Lambda { binding; content } ->
      sprintf "(\\%s %s)" (show_a binding) (e2s content)
  | Tuple v ->
      "(" ^ (List.map ~f:(fun v -> e2s v ^ ", ") v |> String.concat) ^ ")"
  | Constructor (name, v) -> sprintf "(%s %s)" (show_a name) (e2s v)
  | Id v -> show_a v
  | Lit v -> Int.to_string v
