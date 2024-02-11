open! Core
open De_burijn_transform
open Stack_first_language.Irs.Flat
open Stack_first_language.Irs.Ast

let convert = Fn.compose of_ens_tns_stream parse_and_convert
let ty_value = convert "type bool = True () | False ();"

let () =
  assert (
    Map.to_alist ty_value.fn_ty_map
    |> List.equal
         (fun (n1, v1) (n2, v2) -> n1 = n2 && equal_ty Int.equal v1 v2)
         [
           (0, Arrow { i = TupleTy []; o = Id 0 });
           (1, Arrow { i = TupleTy []; o = Id 0 });
         ])

(* let () = *)
(*   Map.to_alist ty_value.ty_def_map *)
(*   |> List.map ~f:(fun (name, y) -> *)
(*          Printf.sprintf "%i : %s" name (show_ty_def Format.pp_print_int y)) *)
(*   |> String.concat ~sep:"\n" |> print_endline *)
