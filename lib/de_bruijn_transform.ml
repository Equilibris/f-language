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

module Namespace = struct
  type ('a, 'b, 'c) t = {
    i2s : (int, 'a, 'b) Base.Map.t;
    s2i : ('a, int, 'c) Base.Map.t;
    req : ('a, int, 'c) Base.Map.t;
    i : int;
  }
  [@@deriving fields]

  let string_namespace =
    let i2s = Map.empty (module Int) in
    let s2i = Map.empty (module String) in
    { i2s; s2i; req = s2i; i = 0 }

  let resolve ({ i2s; s2i; i; req } as id) name =
    match (Map.find s2i name, Map.find req name) with
    | None, Some v -> (id, v)
    | None, None ->
        ( {
            i2s = Map.set ~key:i ~data:name i2s;
            i = i + 1;
            req = Map.set ~key:name ~data:i req;
            s2i;
          },
          i )
    | Some v, _ -> (id, v)

  let assign { i2s; s2i; i; req } name =
    match Map.find req name with
    | Some v ->
        ( {
            i2s = Map.set ~key:v ~data:name i2s;
            s2i = Map.set ~key:name ~data:v s2i;
            i;
            req = Map.remove req name;
          },
          v )
    | None ->
        ( {
            i2s = Map.set ~key:i ~data:name i2s;
            s2i = Map.set ~key:name ~data:i s2i;
            i = i + 1;
            req = Map.remove req name;
          },
          i )

  let scope { s2i; i2s = _; i = _; req = _ } ns = { ns with s2i }

  let base_bind { i2s; s2i; i; req } name =
    ( {
        i2s = Map.set ~key:i ~data:name i2s;
        s2i = Map.set ~key:name ~data:i s2i;
        i = i + 1;
        req;
      },
      i )

  let bind (ns, name) within =
    let ns, i = base_bind ns name in
    let nns, v = within (ns, i) in
    (scope ns nns, v)

  let ( let* ) = bind
end

open Namespace

let rec expr_name_mapper ens = function
  | Id name ->
      let ens, name = resolve ens name in
      (ens, Id name)
  | Bind { name; value; within } ->
      let ens, value = expr_name_mapper ens value in
      let* ens, name = (ens, name) in
      let ens, within = expr_name_mapper ens within in
      (ens, Bind { name; value; within })
  | Lambda { binding; content } ->
      let* ens, binding = (ens, binding) in
      let ens, content = expr_name_mapper ens content in
      (ens, Lambda { binding; content })
  | Constructor (name, value) ->
      let ens, name = resolve ens name in
      let ens, value = expr_name_mapper ens value in
      (ens, Constructor (name, value))
  | Condition { predicate; t_branch; f_branch } ->
      let ens, predicate = expr_name_mapper ens predicate in
      let ens, t_branch = expr_name_mapper ens t_branch in
      let ens, f_branch = expr_name_mapper ens f_branch in
      (ens, Condition { predicate; t_branch; f_branch })
  | Call { callee; arg } ->
      let ens, callee = expr_name_mapper ens callee in
      let ens, arg = expr_name_mapper ens arg in
      (ens, Call { callee; arg })
  | Tuple a ->
      let ens, a =
        List.fold ~init:(ens, [])
          ~f:(fun (ens, last) v ->
            let ens, v = expr_name_mapper ens v in
            (ens, v :: last))
          a
      in
      (ens, Tuple (List.rev a))
  | Lit v -> (ens, Lit v)

let rec ty_name_mapper tns = function
  | Var name ->
      let tns, name = resolve tns name in
      (tns, Var name)
  | Id name ->
      let tns, name = resolve tns name in
      (tns, Id name)
  | Applicative { ty; arg } ->
      let tns, ty = ty_name_mapper tns ty in
      let tns, arg = ty_name_mapper tns arg in
      (tns, Applicative { ty; arg })
  | Arrow { i = input; o = output } ->
      let tns, input = ty_name_mapper tns input in
      let tns, output = ty_name_mapper tns output in
      (tns, Arrow { i = input; o = output })
  | TupleTy tuple ->
      let tns, a =
        List.fold ~init:(tns, [])
          ~f:(fun (tns, last) v ->
            let tns, v = ty_name_mapper tns v in
            (tns, v :: last))
          tuple
      in
      (tns, TupleTy (List.rev a))

let rec top_level_name_mapper ~ens ~tns = function
  | curr :: next ->
      let ens, tns, value =
        match curr with
        | Decl { name; expr } ->
            let ens, name = assign ens name in
            let nens, expr = expr_name_mapper ens expr in
            (scope ens nens, tns, Decl { name; expr })
        | TyDef { name; vars; constructors } ->
            let tns, name = assign tns name in
            let ntns, vars =
              List.fold ~init:(tns, [])
                ~f:(fun (tns, vars) var ->
                  let ns, var = base_bind tns var in
                  (ns, var :: vars))
                vars
            in
            let ens, ntns, constructors =
              List.fold ~init:(ens, ntns, [])
                ~f:(fun (ens, tns, cons) { constructor; ty } ->
                  let ens, constructor = assign ens constructor in
                  let tns, ty = ty_name_mapper tns ty in
                  (ens, tns, { constructor; ty } :: cons))
                constructors
            in
            ( ens,
              scope tns ntns,
              TyDef { name; vars; constructors = List.rev constructors } )
        | DeclTy _ -> (ens, tns, failwith "todo")
      in
      let ens, tns, next = top_level_name_mapper ~ens ~tns next in
      (ens, tns, value :: next)
  | [] -> (ens, tns, [])

module Tests = struct
  open Core.Poly

  let convert code =
    top_level_name_mapper ~tns:string_namespace ~ens:string_namespace code

  let parse_and_convert code =
    convert (Parser_tests.test_parse Parser.top_level code)

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

  let%test _ =
    let _, _, res = parse_and_convert rec_test in
    res
    = [
        Decl
          {
            name = 0;
            expr =
              Lambda
                {
                  Ast.binding = 1;
                  content = Call { callee = Id 2; arg = Id 1 };
                };
          };
        Decl
          {
            name = 2;
            expr =
              Lambda
                { binding = 3; content = Call { callee = Id 0; arg = Id 3 } };
          };
      ]

  let%test _ =
    let _, _, res = parse_and_convert {|type bool = True () | False (); |} in
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
      ]

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
end
