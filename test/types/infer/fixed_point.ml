open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let flat = convert {|
        f = \x (x, f x);
      |}

(* TODO: add rec type, 'a -> ('a * 'b as 'b) *)

let _ =
  let%bind _ = None in
  let%map _, _, ty = gather_top_level flat 0 in

  show_ty Format.pp_print_int ty |> print_endline
