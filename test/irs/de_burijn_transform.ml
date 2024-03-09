open Core
open Core.Poly
open Flang.Irs.De_bruijn_transform
open Flang.Irs.De_bruijn_transform.S
open Flang.Ds.Namespace
open Flang.Irs.Ast

let x = { tns = string_namespace; ens = string_namespace }
let convert code = top_level_name_mapper code x

let parse_and_convert code =
  convert (Parser.test_parse Flang.Parser.top_level code)

let print_src_kvs code =
  let { ens; tns }, result = parse_and_convert code in
  stmts_to_src Int.to_string result |> print_endline;
  Printf.printf "\n";
  i2s ens
  |> Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data);
  Printf.printf "\n";
  i2s tns
  |> Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data)

let rec_test = {|x = \x y x;y = \y x y;|}

let () =
  let _, res = parse_and_convert rec_test in
  assert (
    res
    = [
        Decl
          {
            name = 0;
            expr =
              Lambda
                { binding = 1; content = Call { callee = Id 2; arg = Id 1 } };
          };
        Decl
          {
            name = 2;
            expr =
              Lambda
                { binding = 3; content = Call { callee = Id 0; arg = Id 3 } };
          };
      ])

let () =
  let _, res = parse_and_convert {|type bool = True () | False (); |} in
  assert (
    res
    = [
        TyDef
          {
            name = 0;
            vars = [];
            constructors =
              [
                { constructor = 0; ty = TupleTy [] };
                { constructor = 1; ty = TupleTy [] };
              ];
          };
      ])

let () =
  let _, res =
    parse_and_convert
      {|
        v = match x with
            | x -> x
            | x -> x
            | Const x -> x
            | (x, y) -> x y
            | y -> x y;
      |}
  in

  assert (
    res
    = [
        Decl
          {
            name = 0;
            expr =
              Match
                {
                  scrutinee = Id 1;
                  arms =
                    [
                      (BindingPat 2, Id 2);
                      (BindingPat 3, Id 3);
                      (ConstructorPat (4, BindingPat 5), Id 5);
                      ( TuplePat [ BindingPat 6; BindingPat 7 ],
                        Call { callee = Id 6; arg = Id 7 } );
                      (BindingPat 8, Call { callee = Id 1; arg = Id 8 });
                    ];
                };
          };
      ])

let () =
  let _, res = parse_and_convert {| type x = X (x, ((),)); |} in
  assert (
    res
    = [
        TyDef
          {
            name = 0;
            vars = [];
            constructors =
              [
                {
                  constructor = 0;
                  ty = TupleTy [ Id 0; TupleTy [ TupleTy [] ] ];
                };
              ];
          };
      ])

(* let%test_unit _ = *)
(*   print_src_kvs *)
(* {| *)
        (*       type boolean = True () | False (); *)
        (*       true = True (); *)
        (*       type boolean = True () | False (); *)

        (*       false = False (); *)
        (*       x = \x y x; *)
        (*       y = \y x y; *)

        (*       type nat = Zero () | Succ nat; *)
        (*       type list 'a = Nil () | V ('a, list 'a); *)
        (*     |} *)
