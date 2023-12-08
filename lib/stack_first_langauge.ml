let%test_unit _ =
    let lex = Lexing.from_string "a := b := () : c := b : c" in
    let result = Parser.top_level Lexer.token lex in
    let open Ast in
    show_top_level_decl (List.hd result) |> print_endline
