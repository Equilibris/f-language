open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let flat =
  convert "type bool = True () | False (); true = True (); false = False ();"

let _ =
  let%bind flat, _, ty_true = gather_type flat ty_map 2 in
  let%map _, _, ty_false = gather_type flat ty_map 3 in
  assert (
    equal_ty Int.equal ty_true (Id 0) && equal_ty Int.equal ty_false (Id 0))
(* equal_ty Int.equal ty1 () *)
