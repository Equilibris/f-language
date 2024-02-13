open! Format
open! Flang
open! Flang.Irs.Ast

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
  let () = assert ("a b c" == "a (b) c")

  let () =
    assert (
      "a (b c)"
      = Call { callee = Id "a"; arg = Call { callee = Id "b"; arg = Id "c" } })

  let () = assert ("()" = Tuple [])
  let () = assert ("(a, )" = Tuple [ Id "a" ])
  let () = assert ("(a, b)" = Tuple [ Id "a"; Id "b" ])
  let () = assert ("(a, b, )" = Tuple [ Id "a"; Id "b" ])

  let () =
    assert (
      "match true with | True () -> hello | False () -> no"
      = Match
          {
            scrutinee = Id "true";
            arms =
              [
                (ConstructorPat ("True", TuplePat []), Id "hello");
                (ConstructorPat ("False", TuplePat []), Id "no");
              ];
          })

  let () = assert ("match () with | () -> ()" == "match () with () -> ()")
  let () = assert ("add one let v = one in v" == "(add one) (let v = one in v)")
  let () = assert ("add one \\v v" == "(add one) (\\v v)")

  (* TODO: fix precidence *)
  (* let () = *)
  (*   assert ( *)
  (*     "add one match () with | () -> ()" *)
  (*     == "(add one) (match () with | () -> ())") *)

  (* let () = *)
  (*   assert ( *)
  (*     "add one match () with | () -> ()" *)
  (*     = Call *)
  (*         { *)
  (*           callee = Call { callee = Id "add"; arg = Id "one" }; *)
  (*           arg = Match (Tuple [], [ (TuplePat [], Tuple []) ]); *)
  (*         }) *)
end

module Pat = struct
  let ( == ), ( = ) = make_test_eq Parser.test_pat (show_pat pp_print_string)

  let () =
    assert (
      "Hello Hello ()"
      = ConstructorPat ("Hello", ConstructorPat ("Hello", TuplePat [])))

  let () = assert ("Hello world" = ConstructorPat ("Hello", BindingPat "world"))
  let () = assert ("()" = TuplePat [])
  let () = assert ("(a)" = BindingPat "a")
  let () = assert ("(a,)" = TuplePat [ BindingPat "a" ])
  let () = assert ("(a, b)" = TuplePat [ BindingPat "a"; BindingPat "b" ])
  let () = assert ("(a, b)" == "(a, b,)")

  let () =
    assert (
      "(a, Constructor b)"
      = TuplePat
          [ BindingPat "a"; ConstructorPat ("Constructor", BindingPat "b") ])
end

module Ty = struct
  let ( == ), ( = ) =
    make_test_eq Parser.top_level
      (Core.List.to_string ~f:(show_stmt pp_print_string))

  let () =
    assert (
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
        ])
  (* open Core *)
  (* |> List.map ~f:show_stmt *)
  (* |> List.intersperse ~sep:"\n\n" *)
  (* |> List.fold_left ~f:( ^ ) ~init:"" *)
  (* |> print_endline *)
end
