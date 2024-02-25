open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let flat = convert {|
       id = \x x;
     |}

let _ =
  let%map _, _, id_ty = gather_type flat ty_map 0 in
  assert (equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 0 }))

let flat =
  convert {|
       id = \x x;
       id2 = \x id x;
       id3 = id2;
     |}

let _ =
  let%map flat, _, id_ty = gather_type flat ty_map 0 in
  let%map flat, _, id2_ty = gather_type flat ty_map 2 in
  let%map _, _, id3_ty = gather_type flat ty_map 4 in
  assert (
    equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 0 })
    && equal_ty Int.equal id_ty id2_ty
    && equal_ty Int.equal id_ty id3_ty)
(* show_ty Format.pp_print_int id_ty |> print_endline *)

let _ =
  let flat = convert {| f = \f \x f x; |} in
  let%map _, _, mp_ty = gather_top_level flat 0 in
  let _, mp_ty = canonicalize (Type_map.empty (module Int)) mp_ty in
  assert (
    equal_ty Int.equal mp_ty
      (Arrow
         {
           i = Arrow { i = Var 0; o = Var 1 };
           o = Arrow { i = Var 0; o = Var 1 };
         }))
