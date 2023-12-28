open Format
open Ast

let test_parse parser str =
  let lex = Lexing.from_string str in
  let result = parser Lexer.token lex in
  result

let make_test_eq parser show =
  let parser = test_parse parser in

  let ieq a b =
    if a = b then true
    else (
      print_endline "Expected eq got\n rhs=";
      show a |> print_endline;
      print_endline "lhs=";
      show b |> print_endline;
      false)
  in

  ( (fun a b ->
      let a = parser a in
      let b = parser b in
      ieq a b),
    fun a b ->
      let a = parser a in
      ieq a b )

module Expr = struct
  let ( == ), ( = ) = make_test_eq Parser.test_expr (show_expr pp_print_string)
  let%test _ = "a b c" == "a (b) c"

  let%test _ =
    "a (b c)"
    = Call { callee = Id "a"; arg = Call { callee = Id "b"; arg = Id "c" } }

  let%test _ = "()" = Tuple []
  let%test _ = "(a, )" = Tuple [ Id "a" ]
  let%test _ = "(a, b)" = Tuple [ Id "a"; Id "b" ]
  let%test _ = "(a, b, )" = Tuple [ Id "a"; Id "b" ]

  let%test _ =
    "if a then b else c"
    = Condition { predicate = Id "a"; t_branch = Id "b"; f_branch = Id "c" }

  let%test _ =
    "Hello if a then b else c"
    = Constructor
        ( "Hello",
          Condition { predicate = Id "a"; t_branch = Id "b"; f_branch = Id "c" }
        )
end

module Ty = struct
  let ( == ), ( = ) =
    make_test_eq Parser.top_level
      (Core.List.to_string ~f:(show_stmt pp_print_string))

  let%test _ =
    {|
        type hello 'a 'b =
           | World ('a, 'b)
           | Other 'b;
        id :: 'a -> 'a;
        id = id;
       |}
    = [
        TyDef
          {
            name = "hello";
            vars = [ "a"; "b" ];
            constructors =
              [
                { constructor = "World"; ty = TupleTy [ Var "a"; Var "b" ] };
                { constructor = "Other"; ty = Var "b" };
              ];
          };
        DeclTy { name = "id"; ty = Arrow { i = Var "a"; o = Var "a" } };
        Decl { name = "id"; expr = Id "id" };
      ]
  (* open Core *)
  (* |> List.map ~f:show_stmt *)
  (* |> List.intersperse ~sep:"\n\n" *)
  (* |> List.fold_left ~f:( ^ ) ~init:"" *)
  (* |> print_endline *)
end
