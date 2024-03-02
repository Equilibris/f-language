open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)
let eset = Set.empty (module Int)

let _ =
  let flat = convert "" in
  let%map _, ty_map, _, true_ty = infer_pat flat ty_map eset (BindingPat 1) in
  assert (
    equal_ty Int.equal true_ty (Var 0)
    && equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (Var 0))

let _ =
  let flat = convert {|
          type bool = True ();
        |} in
  let%bind _, ty_map, _, true_ty =
    infer_pat flat ty_map eset (ConstructorPat (0, BindingPat 1))
  in
  let%map alt_ty = Map.find ty_map.id_ty_map 1 in
  assert (
    equal_ty Int.equal true_ty (Id 0) && equal_ty Int.equal alt_ty (TupleTy []))

(* TODO: *)
(* let _ = *)
(* let flat = convert {| *)
   (*           type bool = True ((), ((),)); *)
   (*         |} in *)
(*   let%bind _, ty_map, _, match_ty = *)
(*     infer_pat flat ty_map eset (ConstructorPat (0, BindingPat 1)) *)
(*   in *)
(*   let%map alt_ty = Map.find ty_map.id_ty_map 1 in *)

(*   print_endline "hello"; *)
(*   show_ty Format.pp_print_int match_ty |> print_endline; *)
(*   show_ty Format.pp_print_int alt_ty |> print_endline; *)
(*   print_endline "end hello" *)
(* assert ( *)
(*   equal_ty Int.equal true_ty (Id 0) *)
(*   && equal_ty Int.equal alt_ty *)
(*        (TupleTy [ TupleTy []; TupleTy [ TupleTy [] ] ])) *)

(* let _ = *)
(* let flat = convert {| *)
   (*           type bool = True (); *)
   (*         |} in *)
(*   let%map _, ty_map, _, true_ty = *)
(*     infer_pat flat ty_map eset *)
(*       (TuplePat [ ConstructorPat (0, BindingPat 1); TuplePat [] ]) *)
(*   in *)
(*   assert ( *)
(*     equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (TupleTy []) (* && *) *)
(*     && equal_ty Int.equal true_ty (TupleTy [ Id 0; TupleTy [] ])) *)

let _ =
  let flat = convert {|
       type x = X (x, ((),));
     |} in
  let%map _, ty_map, _, t =
    infer_pat flat ty_map eset
      (ConstructorPat (0, TuplePat [ BindingPat 1; BindingPat 10 ]))
  in
  assert (
    equal_ty Int.equal t (Id 0)
    && equal_ty Int.equal (Map.find_exn ty_map.id_ty_map 1) (Id 0)
    && equal_ty Int.equal
         (Map.find_exn ty_map.id_ty_map 10)
         (TupleTy [ TupleTy [] ]))
