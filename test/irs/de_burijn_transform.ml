open Core
open Core.Poly
open Stack_first_language.Irs.De_bruijn_transform
open Stack_first_language.Irs.Namespace
open Stack_first_language.Irs.Ast

let convert code =
  top_level_name_mapper ~tns:string_namespace ~ens:string_namespace code

let parse_and_convert code =
  convert (Parser.test_parse Stack_first_language.Parser.top_level code)

let print_src_kvs code =
  let ens, tns, result = parse_and_convert code in
  stmts_to_src Int.to_string result |> print_endline;
  Printf.printf "\n";
  i2s ens
  |> Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data);
  Printf.printf "\n";
  i2s tns
  |> Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data)

(* let _ = let _,_,  *)
(*     top_level_name_mapper ~tns:string_namespace ~ens:string_namespace  *)

(* let%test_unit _ = *)
(*   Parser_tests.test_parse Parser.top_level {|x = \x y x;y = \y x y;|} *)
(*   |> List.map ~f:(show_stmt Format.pp_print_string) *)
(*   |> List.iter ~f:print_endline *)

let rec_test = {|x = \x y x;y = \y x y;|}

let _ =
  let _, _, res = parse_and_convert rec_test in
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

let _ =
  let _, _, res = parse_and_convert {|type bool = True () | False (); |} in
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
