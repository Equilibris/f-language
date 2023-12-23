open Ast

module ParsingTests = struct
  let test_parse parser str =
    let lex = Lexing.from_string str in
    let result = parser Lexer.token lex in
    result

  let make_test_eq parser show =
    let parser = test_parse parser in

    let ieq a b =
      if a = b then true
      else (
        Printf.printf "Expected eq got\nrhs = (%s)\nlhs = (%s)\n%!" (show a)
          (show b);
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
    let ( == ), ( = ) = make_test_eq Parser.test_expr Ast.show_expr
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
            Condition
              { predicate = Id "a"; t_branch = Id "b"; f_branch = Id "c" } )
  end

  module Ty = struct
    let ( == ), ( = ) = make_test_eq Parser.test_ty Ast.show_ty
    let%test _ = "a b c" == "(a b) c"
    let%test _ = "a -> b -> c" == "a -> (b -> c)"
    let%test _ = "(a b) -> (a b)" == "a b -> (a b)"

    let%test _ =
      "'a -> b -> b 'a"
      = Arrow
          {
            i = Var "'a";
            o =
              Arrow
                { i = Id "b"; o = Applicative { ty = Id "b"; arg = Var "'a" } };
          }

    let%test _ =
      "(a, b -> b)" = TupleTy [ Id "a"; Arrow { i = Id "b"; o = Id "b" } ]

    let%test _ = "()" = TupleTy []
    let%test _ = "(a, )" = TupleTy [ Id "a" ]
  end

  module TopLevel = struct
    let _, ( = ) =
      make_test_eq Parser.top_level (Core.List.to_string ~f:Ast.show_stmt)

    let%test _ =
      {|
          data hello
              | World ('a, 'b)
              | Other 'b;
          id :: 'a -> 'a;
          id = id;
      |}
      = [
          Ast.TyDef
            {
              Ast.name = "hello";
              constructors =
                [
                  {
                    Ast.constructor = "World";
                    ty = Ast.TupleTy [ Ast.Var "'a"; Ast.Var "'b" ];
                  };
                  { Ast.constructor = "Other"; ty = Ast.Var "'b" };
                ];
            };
          Ast.DeclTy
            {
              Ast.name = "id";
              ty = Ast.Arrow { Ast.i = Ast.Var "'a"; o = Ast.Var "'a" };
            };
          Ast.Decl { Ast.name = "id"; expr = Ast.Id "id" };
        ]
    (* open Core *)
    (* |> List.map ~f:show_stmt *)
    (* |> List.intersperse ~sep:"\n\n" *)
    (* |> List.fold_left ~f:( ^ ) ~init:"" *)
    (* |> print_endline *)
  end
end
