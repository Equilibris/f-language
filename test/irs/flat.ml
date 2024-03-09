open! Core
open De_burijn_transform
open Flang.Irs.Flat
open Flang.Irs.Ast

let convert = Fn.compose of_ens_tns_stream parse_and_convert

let convert_with_map x =
  let s, v = parse_and_convert x in
  (s, of_ens_tns_stream (s, v))

let ty_value = convert "type bool = True () | False (); type x = X ((), ((),));"

let () =
  assert (
    Map.to_alist ty_value.fn_ty_map
    |> List.equal
         (fun (n1, v1) (n2, v2) -> n1 = n2 && equal_ty Int.equal v1 v2)
         [
           (0, Arrow { i = TupleTy []; o = Id 0 });
           (1, Arrow { i = TupleTy []; o = Id 0 });
           ( 2,
             Arrow
               { i = TupleTy [ TupleTy []; TupleTy [ TupleTy [] ] ]; o = Id 1 }
           );
         ])
