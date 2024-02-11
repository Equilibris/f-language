open Core
open Type
open Expr

type 'a ty_def = {
  name : 'a;
  vars : 'a list;
  constructors : 'a constructor_def list;
}

and 'a constructor_def = { constructor : 'a; ty : 'a ty } [@@deriving show]

type ('a, 'expr) decl_stmt = { name : 'a; expr : 'expr } [@@deriving show]
type 'a decl_ty_stmt = { name : 'a; ty : 'a ty } [@@deriving show]

type ('a, 'expr) top_level =
  | Decl of ('a, 'expr) decl_stmt
  | DeclTy of 'a decl_ty_stmt
  | TyDef of 'a ty_def

and 'a stmt = ('a, 'a expr) top_level [@@deriving show]

let stmt_to_src show_a = function
  | Decl { name; expr } ->
      Printf.sprintf "%s = %s;" (show_a name) (expr_to_src show_a expr)
  | TyDef { name; vars; constructors } ->
      Printf.sprintf "type %s %s=%s;" (show_a name)
        (List.map ~f:(fun v -> Printf.sprintf "'%s " (show_a v)) vars
        |> String.concat)
        (List.map
           ~f:(fun { constructor; ty } ->
             Printf.sprintf " | %s %s" (show_a constructor)
               (ty_to_src show_a ty))
           constructors
        |> String.concat)
  | DeclTy { name; ty } ->
      Printf.sprintf "%s :: %s;" (show_a name) (ty_to_src show_a ty)
