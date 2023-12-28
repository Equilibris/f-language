open Core
open Ast

(** De Bruijn index Transformations *)

(*
 TODO:
 - [ ] Look into using binding overriding to make this code a bit nicer
 *)

(*
 Variable name index

 - If a var starts with e or t it is expr or type respectivley
 - i2s and s2i are int to string and v.v.
 - xreq is a requests system to manage possibly undefined variables
 *)

let name_mapper ~i2s ~s2i ~i ~req name =
  match (Map.find s2i name, Map.find req name) with
  | None, Some v -> (i2s, i, req, v)
  | None, None ->
      (Map.set ~key:i ~data:name i2s, i + 1, Map.set ~key:name ~data:i req, i)
  | Some v, _ -> (i2s, i, req, v)

let name_assigner ~i2s ~s2i ~i ~req name =
  match Map.find req name with
  | Some v ->
      ( Map.set ~key:v ~data:name i2s,
        Map.set ~key:name ~data:v s2i,
        i,
        Map.remove req name,
        v )
  | None ->
      ( Map.set ~key:i ~data:name i2s,
        Map.set ~key:name ~data:i s2i,
        i + 1,
        Map.remove req name,
        i )

let rec expr_name_mapper ~ei2s ~es2i ~ei ~ereq = function
  | Id name ->
      let ei2s, ei, ereq, name =
        name_mapper ~i2s:ei2s ~s2i:es2i ~i:ei ~req:ereq name
      in
      (ei2s, es2i, ei, ereq, Id name)
  | Bind { name; value; within } ->
      let ei2s, _, ei, ereq, value =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq value
      in
      let ei2s, nes2i, ei, name =
        ( Map.set ~key:ei ~data:name ei2s,
          Map.set ~key:name ~data:ei es2i,
          ei + 1,
          ei )
      in
      let ei2s, _, ei, ereq, within =
        expr_name_mapper ~ei2s ~es2i:nes2i ~ei ~ereq within
      in
      (ei2s, es2i, ei, ereq, Bind { name; value; within })
  | Lambda { binding; content } ->
      let ei2s, nes2i, ei, binding =
        ( Map.set ~key:ei ~data:binding ei2s,
          Map.set ~key:binding ~data:ei es2i,
          ei + 1,
          ei )
      in
      let ei2s, _, ei, ereq, content =
        expr_name_mapper ~ei2s ~es2i:nes2i ~ei ~ereq content
      in
      (ei2s, es2i, ei, ereq, Lambda { binding; content })
  | Constructor (name, value) ->
      let ei2s, ei, ereq, name =
        name_mapper ~i2s:ei2s ~s2i:es2i ~i:ei ~req:ereq name
      in
      let ei2s, es2i, ei, ereq, value =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq value
      in
      (ei2s, es2i, ei, ereq, Constructor (name, value))
  | Condition { predicate; t_branch; f_branch } ->
      let ei2s, _, ei, ereq, predicate =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq predicate
      in
      let ei2s, _, ei, ereq, t_branch =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq t_branch
      in
      let ei2s, _, ei, ereq, f_branch =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq f_branch
      in
      (ei2s, es2i, ei, ereq, Condition { predicate; t_branch; f_branch })
  | Call { callee; arg } ->
      let ei2s, _, ei, ereq, callee =
        expr_name_mapper ~ei2s ~es2i ~ei ~ereq callee
      in
      let ei2s, _, ei, ereq, arg = expr_name_mapper ~ei2s ~es2i ~ei ~ereq arg in
      (ei2s, es2i, ei, ereq, Call { callee; arg })
  | Tuple a ->
      let ei2s, ei, ereq, a =
        List.fold ~init:(ei2s, ei, ereq, [])
          ~f:(fun (ei2s, ei, ereq, last) v ->
            let ei2s, _, ei, ereq, v =
              expr_name_mapper ~ei2s ~es2i ~ei ~ereq v
            in
            (ei2s, ei, ereq, v :: last))
          a
      in
      (ei2s, es2i, ei, ereq, Tuple (List.rev a))
  | Lit v -> (ei2s, es2i, ei, ereq, Lit v)

let rec ty_name_mapper ~ti2s ~ts2i ~ti ~treq = function
  | Var name ->
      let ti2s, ti, treq, name =
        name_mapper ~i2s:ti2s ~s2i:ts2i ~i:ti ~req:treq name
      in
      (ti2s, ts2i, ti, treq, Var name)
  | Id name ->
      let ti2s, ti, treq, name =
        name_mapper ~i2s:ti2s ~s2i:ts2i ~i:ti ~req:treq name
      in
      (ti2s, ts2i, ti, treq, Id name)
  | Applicative { ty; arg } ->
      let ti2s, ts2i, ti, treq, ty = ty_name_mapper ~ti2s ~ts2i ~ti ~treq ty in
      let ti2s, ts2i, ti, treq, arg =
        ty_name_mapper ~ti2s ~ts2i ~ti ~treq arg
      in
      (ti2s, ts2i, ti, treq, Applicative { ty; arg })
  | Arrow { i = input; o = output } ->
      let ti2s, ts2i, ti, treq, input =
        ty_name_mapper ~ti2s ~ts2i ~ti ~treq input
      in
      let ti2s, ts2i, ti, treq, output =
        ty_name_mapper ~ti2s ~ts2i ~ti ~treq output
      in
      (ti2s, ts2i, ti, treq, Arrow { i = input; o = output })
  | TupleTy tuple ->
      let ti2s, ti, ereq, a =
        List.fold ~init:(ti2s, ti, treq, [])
          ~f:(fun (ti2s, ti, treq, last) v ->
            let ti2s, _, ti, treq, v = ty_name_mapper ~ti2s ~ts2i ~ti ~treq v in
            (ti2s, ti, treq, v :: last))
          tuple
      in
      (ti2s, ts2i, ti, ereq, TupleTy (List.rev a))

let rec top_level_name_mapper ~ei2s ~ti2s ~es2i ~ts2i ~ei ~ti ~ereq ~treq =
  function
  | curr :: next ->
      let ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, value =
        match curr with
        | Decl { name; expr } ->
            let ei2s, es2i, ei, ereq, name =
              name_assigner ~i2s:ei2s ~s2i:es2i ~i:ei ~req:ereq name
            in
            let ei2s, _, ei, ereq, expr =
              expr_name_mapper ~ei2s ~es2i ~ei ~ereq expr
            in
            (ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, Decl { name; expr })
        | TyDef { name; vars; constructors } ->
            let ti2s, ts2i, ti, treq, name =
              name_assigner ~i2s:ti2s ~s2i:ts2i ~i:ti ~req:treq name
            in
            let ti2s, nts2i, ti, vars =
              List.fold ~init:(ti2s, ts2i, ti, [])
                ~f:(fun (ti2s, ts2i, ti, vars) var ->
                  ( Map.set ~key:ti ~data:var ti2s,
                    Map.set ~key:var ~data:ti ts2i,
                    ti + 1,
                    ti :: vars ))
                vars
            in
            let ei2s, ti2s, es2i, _, ei, ti, ereq, treq, constructors =
              List.fold
                ~init:(ei2s, ti2s, es2i, nts2i, ei, ti, ereq, treq, [])
                ~f:(fun
                    (ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, cons)
                    { constructor; ty }
                  ->
                  let ei2s, es2i, ei, ereq, constructor =
                    name_assigner ~i2s:ei2s ~s2i:es2i ~i:ei ~req:ereq
                      constructor
                  in
                  let ti2s, ts2i, ti, treq, ty =
                    ty_name_mapper ~ti2s ~ts2i ~ti ~treq ty
                  in
                  ( ei2s,
                    ti2s,
                    es2i,
                    ts2i,
                    ei,
                    ti,
                    ereq,
                    treq,
                    { constructor; ty } :: cons ))
                constructors
            in
            ( ei2s,
              ti2s,
              es2i,
              ts2i,
              ei,
              ti,
              ereq,
              treq,
              TyDef { name; vars; constructors = List.rev constructors } )
        | DeclTy _ ->
            (ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, failwith "todo")
      in
      let ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, next =
        top_level_name_mapper ~ei2s ~ti2s ~es2i ~ts2i ~ei ~ti ~ereq ~treq next
      in
      (ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, value :: next)
  | [] -> (ei2s, ti2s, es2i, ts2i, ei, ti, ereq, treq, [])

module Tests = struct
  let boot code =
    let code = Parser_tests.test_parse Parser.top_level code in

    let i2s = Map.empty (module Int) in
    let s2i = Map.empty (module String) in
    let ei2s, ti2s, _, _, _, _, _, _, result =
      top_level_name_mapper ~ei2s:i2s ~ti2s:i2s ~es2i:s2i ~ts2i:s2i ~ei:0 ~ti:0
        ~ereq:s2i ~treq:s2i code
    in
    (ei2s, ti2s, result)

  let print_src_kvs code =
    let ei2s, ti2s, result = boot code in
    List.iter ~f:(fun x -> stmt_to_src Int.to_string x |> print_endline) result;
    Printf.printf "\n";
    Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data) ei2s;
    Printf.printf "\n";
    Map.iteri ~f:(fun ~key ~data -> Printf.printf "%i = %s\n%!" key data) ti2s

  let%test_unit _ =
    print_src_kvs
      {|
    type boolean = True () | False ();
    true = True ();
    false = False ();
    type boolean2 = True () | False ();

    x = \x y x;
    y = \y x y;

    type nat = Zero () | Succ nat;
    type list 'a = Nil () | V ('a, list 'a);
  |}
end
