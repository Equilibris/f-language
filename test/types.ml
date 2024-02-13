open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = TypeMap.empty (module Int)

module TyVarSimp = struct
  let () =
    let _, nt = canonicalize ty_map (Arrow { i = Var 10; o = Var 9 }) in
    assert (equal_ty Int.equal nt (Arrow { i = Var 0; o = Var 1 }))

  let () =
    let _, nt = canonicalize ty_map (Arrow { i = Var 100; o = Var 100 }) in
    assert (equal_ty Int.equal nt (Arrow { i = Var 0; o = Var 0 }))

  let () =
    let ty_map, nt1 =
      canonicalize ty_map (Arrow { i = Var 100; o = Var 100 })
    in
    let _, nt2 = canonicalize ty_map (Arrow { i = Var 100; o = Var 101 }) in
    assert (
      equal_ty Int.equal nt1 (Arrow { i = Var 0; o = Var 0 })
      && equal_ty Int.equal nt2 (Arrow { i = Var 1; o = Var 2 }))
end

module LambdalessNonRec = struct
  let flat =
    convert "type bool = True () | False (); true = True (); false = False ();"

  let _ =
    let%bind flat, _, ty_true = gather_type flat ty_map 2 in
    let%map _, _, ty_false = gather_type flat ty_map 3 in
    assert (
      equal_ty Int.equal ty_true (Id 0) && equal_ty Int.equal ty_false (Id 0))
  (* equal_ty Int.equal ty1 () *)
end

module LambdaNonRec = struct
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
end

module Lambda = struct
  let flat = convert {|
      loop = \x loop x;
  |}

  let _ =
    let%map _, id_ty = gather_top_level flat 0 in
    let _, id_ty = canonicalize (TypeMap.empty (module Int)) id_ty in
    assert (equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Var 1 }))

  let flat = convert {|
      f = \x g x;
      g = \x f x;
  |}

  let _ =
    Map.keys flat.fn_def_map
    |> List.map ~f:(fun v ->
           let%map _, ty = gather_top_level flat v in
           let _, ty = canonicalize (TypeMap.empty (module Int)) ty in

           assert (equal_ty Int.equal ty (Arrow { i = Var 0; o = Var 1 })))
end

module FixedPoint = struct
  let flat = convert {|
      f = \x (x, f x);
  |}

  let _ =
    let%map _, ty = gather_top_level flat 0 in
    show_ty Format.pp_print_int ty |> print_endline
end
