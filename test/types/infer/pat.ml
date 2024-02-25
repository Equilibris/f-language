open! Core
open Flang.Types
open Flang.Irs.Ast
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)
let eset = Set.empty (module Int)

let _ =
  let flat = convert {|
       type bool = True ();
     |} in
  let%map _, _, _, true_ty =
    infer_pat flat ty_map eset
      (TuplePat [ ConstructorPat (0, BindingPat 1); TuplePat [] ])
  in
  print_ty "hi" true_ty
(* assert ( *)
(*   equal_ty Int.equal (Map.find_exn ty_map.var_map 0) (TupleTy []) (* && *) *)
(*   && equal_ty Int.equal true_ty (TupleTy [ Id 0; TupleTy [] ])) *)

(* let _ = *)
(* let flat = convert {| *)
   (*     type x = X (x, ((),)); *)
   (*   |} in *)
(*   let%map _, ty_map, _, t = *)
(*     infer_pat flat ty_map eset *)
(*       (ConstructorPat (0, TuplePat [ BindingPat 1; BindingPat 10 ])) *)
(*   in *)
(*   print_ty_map ty_map; *)
(*   print_ty "t" t *)
