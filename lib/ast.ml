open Core

(*
 TODO:
 - [ ] Scoped co-recursion is impossible in the current setup
 - [ ] use (type <expr>) in types to represent type of relations,
       useful for when we add module inference
 *)

type 'a arrow_ty = { i : 'a ty; o : 'a ty }
and 'a applicative_ty = { ty : 'a ty; arg : 'a ty }
and 'a tuple_ty = 'a ty list

and 'a ty =
  | Id of 'a
  (* Simple SystemF *)
  | Applicative of 'a applicative_ty
  | TupleTy of 'a tuple_ty
  | Var of 'a
  | Arrow of 'a arrow_ty
[@@deriving show]

type 'a bind_expr = { name : 'a; value : 'a expr; within : 'a expr }

and 'a condition_expr = {
  predicate : 'a expr;
  t_branch : 'a expr;
  f_branch : 'a expr;
}

and 'a call_expr = { callee : 'a expr; arg : 'a expr }
and 'a lambda_expr = { binding : 'a; content : 'a expr }
and 'a tuple_expr = 'a expr list
and 'a constructor_expr = 'a * 'a expr
and lit_expr = int

and 'a expr =
  | Bind of 'a bind_expr
  | Condition of 'a condition_expr
  | Call of 'a call_expr
  | Lambda of 'a lambda_expr
  | Tuple of 'a tuple_expr
  | Constructor of 'a constructor_expr
  | Id of 'a
  | Lit of lit_expr
[@@deriving show]

type 'a ty_def = {
  name : 'a;
  vars : 'a list;
  constructors : 'a constructor_def list;
}

and 'a constructor_def = { constructor : 'a; ty : 'a ty } [@@deriving show]

type 'a decl_stmt = { name : 'a; expr : 'a expr } [@@deriving show]
type 'a decl_ty_stmt = { name : 'a; ty : 'a ty } [@@deriving show]

type 'a stmt =
  | Decl of 'a decl_stmt
  | DeclTy of 'a decl_ty_stmt
  | TyDef of 'a ty_def
[@@deriving show]

let rec expr_to_src show_a = function
  | Bind { name; value; within } ->
      Printf.sprintf "(let %s = %s in %s)" (show_a name)
        (expr_to_src show_a value)
        (expr_to_src show_a within)
  | Condition { predicate; t_branch; f_branch } ->
      Printf.sprintf "(if %s then %s else %s)"
        (expr_to_src show_a predicate)
        (expr_to_src show_a t_branch)
        (expr_to_src show_a f_branch)
  | Call { callee; arg } ->
      Printf.sprintf "(%s %s)"
        (expr_to_src show_a callee)
        (expr_to_src show_a arg)
  | Lambda { binding; content } ->
      Printf.sprintf "(\\%s %s)" (show_a binding) (expr_to_src show_a content)
  | Tuple v ->
      "("
      ^ (List.map ~f:(fun v -> expr_to_src show_a v ^ ", ") v |> String.concat)
      ^ ")"
  | Constructor (name, v) ->
      Printf.sprintf "(%s %s)" (show_a name) (expr_to_src show_a v)
  | Id v -> show_a v
  | Lit v -> Int.to_string v

let rec ty_to_src show_a = function
  | Applicative { ty; arg } ->
      Printf.sprintf "(%s %s)" (ty_to_src show_a ty) (ty_to_src show_a arg)
  | Id v -> show_a v
  | TupleTy v ->
      "("
      ^ (List.map ~f:(fun v -> ty_to_src show_a v ^ ", ") v |> String.concat)
      ^ ")"
  | Var v -> Printf.sprintf "'%s" (show_a v)
  | Arrow { i; o } ->
      Printf.sprintf "(%s -> %s)" (ty_to_src show_a i) (ty_to_src show_a o)

let stmt_to_src show_a = function
  | Decl { name; expr } ->
      Printf.sprintf "%s = %s;" (show_a name) (expr_to_src show_a expr)
  | TyDef { name; vars; constructors } ->
      Printf.sprintf "type %s %s=%s" (show_a name)
        (List.map ~f:(fun v -> Printf.sprintf "'%s " (show_a v)) vars
        |> String.concat)
        (List.map
           ~f:(fun { constructor; ty } ->
             Printf.sprintf " | %s %s" (show_a constructor)
               (ty_to_src show_a ty))
           constructors
        |> String.concat)
  | DeclTy { name; ty } ->
      Printf.sprintf "%s :: %s" (show_a name) (ty_to_src show_a ty)
