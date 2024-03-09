open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax
open Canonicalize

let ty_map = Type_map.empty (module Int)

let flat_ir = convert {|
       id = \x x;
     |}

let _ =
  let%map _, id_ty = gather_type 0 { flat_ir; ty_map } in
  assert (equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 0 }))

let _ =
  let flat_ir =
    convert {|
       id = \x x;
       id2 = \x id x;
       id3 = id2;
     |}
  in
  let%map { flat_ir; _ }, id_ty = gather_type 0 { flat_ir; ty_map } in
  let%map { flat_ir; _ }, id2_ty = gather_type 2 { flat_ir; ty_map } in
  let%map _, id3_ty = gather_type 4 { flat_ir; ty_map } in
  assert (
    equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 0 })
    && equal_ty Int.equal id_ty id2_ty
    && equal_ty Int.equal id_ty id3_ty)
(* show_ty Format.pp_print_int id_ty |> print_endline *)

let _ =
  let flat = convert {| f = \f \x f x; |} in
  let%map _, mp_ty = gather_top_level flat 0 in
  let _, mp_ty = canonicalize mp_ty in
  assert (
    equal_ty Int.equal mp_ty
      (Arrow
         {
           i = Arrow { i = Var 0; o = Var 1 };
           o = Arrow { i = Var 0; o = Var 1 };
         }))
