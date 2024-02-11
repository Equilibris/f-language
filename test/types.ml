open! Core
open Stack_first_language.Types
open Stack_first_language.Irs.Ast
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

  let flat = convert {|
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
