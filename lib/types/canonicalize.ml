open Core
open Irs.Ast

let canonicalize ty_map ty =
  let imap = Hashtbl.create (module Int) in
  let rec traverse ty_map = function
    | Arrow { i; o } ->
        let ty_map, i = traverse ty_map i in
        let ty_map, o = traverse ty_map o in
        (ty_map, Arrow { i; o })
    | Id _ as x -> (ty_map, x)
    | Applicative { ty; arg } ->
        let ty_map, ty = traverse ty_map ty in
        let ty_map, arg = traverse ty_map arg in
        (ty_map, Applicative { ty; arg })
    | TupleTy typ ->
        let ty_map, x =
          List.fold_map ~init:ty_map
            ~f:(fun ty_map nx ->
              let ty_map, nx = traverse ty_map nx in
              (ty_map, nx))
            typ
        in
        (ty_map, TupleTy x)
    | Var v -> (
        match Hashtbl.find imap v with
        | Some v -> (ty_map, v)
        | None ->
            let ty_map, cursor = Type_map.bind_var ty_map in
            Hashtbl.set imap ~key:v ~data:cursor;
            (ty_map, cursor))
  in
  traverse ty_map ty
