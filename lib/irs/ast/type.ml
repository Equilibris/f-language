open Core

type 'a arrow_ty = { i : 'a ty; o : 'a ty }
and 'a applicative_ty = { ty : 'a ty; arg : 'a ty }
and 'a tuple_ty = 'a ty list
(* and 'a rec_ty = { binding : 'a; domain : 'a ty } *)

and 'a ty =
  | Id of 'a
  (* Simple SystemF *)
  | Applicative of 'a applicative_ty
  | TupleTy of 'a tuple_ty
  | Var of 'a
  | Arrow of 'a arrow_ty (* | Rec of 'a rec_ty *)
[@@deriving show, eq]

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
(* | Rec { binding; domain } -> *)
(*     Printf.sprintf "(fix %s = %s)" (show_a binding) (ty_to_src show_a domain) *)
