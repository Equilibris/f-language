open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let flat_ir =
  convert "type bool = True () | False (); true = True (); false = False ();"

let _ =
  let%bind { flat_ir; ty_map = _ }, ty_true =
    gather_type 2 { flat_ir; ty_map }
  in
  let%map _, ty_false = gather_type 3 { flat_ir; ty_map } in
  assert (
    equal_ty Int.equal ty_true (Id 0) && equal_ty Int.equal ty_false (Id 0))
(* equal_ty Int.equal ty1 () *)
