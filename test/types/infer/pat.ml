open! Core
open Flang.Types
open Flang.Irs.Ast
open Flang.Types.State
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)
let eset = Set.empty (module Int)

let _ =
  let flat_ir = convert "" in
  let%map { flat_ir = _; ty_map }, (_, true_ty) =
    infer_pat eset (BindingPat 1) { flat_ir; ty_map }
  in
  assert (
    equal_ty Int.equal true_ty (Var 0)
    && equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (Var 0))

let _ =
  let flat_ir = convert {|
  type bool = True ();
|} in
  let%bind { flat_ir = _; ty_map }, (_, true_ty) =
    infer_pat eset (ConstructorPat (0, BindingPat 1)) { flat_ir; ty_map }
  in
  let%map alt_ty = Map.find ty_map.id_ty_map 1 in
  assert (
    equal_ty Int.equal true_ty (Id 0) && equal_ty Int.equal alt_ty (TupleTy []))

let _ =
  let flat_ir =
    convert {|
             type bool = True ((), ((),));
           |}
  in
  let%bind { ty_map; flat_ir = _ }, (_, match_ty) =
    infer_pat eset (ConstructorPat (0, BindingPat 1)) { flat_ir; ty_map }
  in
  let%map alt_ty = Map.find ty_map.id_ty_map 1 in

  assert (
    equal_ty Int.equal match_ty (Id 0)
    && equal_ty Int.equal alt_ty
         (TupleTy [ TupleTy []; TupleTy [ TupleTy [] ] ]))

let _ =
  let flat_ir = convert {|
        type bool = True ();
    |} in
  let%map { ty_map; flat_ir = _ }, (_, match_ty) =
    infer_pat eset
      (TuplePat [ ConstructorPat (0, BindingPat 1); TuplePat [] ])
      { flat_ir; ty_map }
  in
  assert (
    equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (TupleTy []) (* && *)
    && equal_ty Int.equal match_ty (TupleTy [ Id 0; TupleTy [] ]))

let _ =
  let flat_ir = convert {|
       type x = X (x, ((),));
     |} in
  let%map { ty_map; flat_ir = _ }, (_, t) =
    infer_pat eset
      (ConstructorPat (0, TuplePat [ BindingPat 1; BindingPat 10 ]))
      { flat_ir; ty_map }
  in
  assert (
    equal_ty Int.equal t (Id 0)
    && equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (Id 0)
    && equal_ty Int.equal
         (Map.find_exn ty_map.id_ty_map 10)
         (TupleTy [ TupleTy [] ]))

let _ =
  let flat_ir = convert {|
    type box 'a = Box 'a;
  |} in
  let%bind { ty_map; flat_ir = _ }, (_, match_ty) =
    infer_pat eset
      (ConstructorPat (0, ConstructorPat (0, BindingPat 1)))
      { flat_ir; ty_map }
  in
  let alt_ty = Map.find_exn ty_map.id_ty_map 1 in
  let%map match_ty = replace_var_deep match_ty ty_map.var_map in
  assert (
    equal_ty Int.equal match_ty
      (Applicative { ty = Id 0; arg = Applicative { ty = Id 0; arg = Var 1 } })
    && equal_ty Int.equal alt_ty (Var 1))
