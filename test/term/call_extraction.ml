open! Core
open Irs.Flat
open Flang.Irs.Ast
open Flang.Term.Call_extraction

let _ =
  let flat_ir =
    convert
      {|
        type nat = Z () | S nat;

        z = \x \y match x with Z () -> z_alt (Z()) (Z()) | S n -> z n y;

    |}
  in
  let { name = _; expr } = Map.find_exn flat_ir.fn_def_map 2 in
  let calls = extract_calls expr in

  assert (
    Core.Poly.(
      Hashtbl.to_alist calls
      (* Dont know if Hashtbl is deterministic *)
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      = [
          (2, [ [ Id 4; Id 6 ] ]);
          (5, [ [ Constructor (0, Tuple []); Constructor (0, Tuple []) ] ]);
        ]))
(* |> List.iter ~f:(fun (k, v) -> *)
(*        printf "%i\t= %s\n\n" k *)
(*          (List.map v ~f:(fun v -> *)
(*               List.map ~f:(show_expr Format.pp_print_int) v *)
(*               |> String.concat ~sep:" ") *)
(*          |> String.concat ~sep:"\n \t  ")) *)
