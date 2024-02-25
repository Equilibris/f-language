open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let flat = convert {|
  loop = \x loop x;
|}

let _ =
  let%map _, _, id_ty = gather_top_level flat 0 in
  let _, id_ty = canonicalize (Type_map.empty (module Int)) id_ty in
  assert (equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 1 }))

let flat = convert {|
  f = \x g x;
  g = \x f x;
|}

let _ =
  Map.keys flat.fn_def_map
  |> List.map ~f:(fun v ->
         let%map _, _, ty = gather_top_level flat v in
         let _, ty = canonicalize (Type_map.empty (module Int)) ty in

         assert (equal_ty Int.equal ty (Arrow { i = Var 0; o = Var 1 })))
