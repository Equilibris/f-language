open Core
open Irs.Ast
open Ds.State.Let_syntax

let canonicalize ty =
  let imap = Hashtbl.create (module Int) in
  let rec traverse = function
    | Arrow { i; o } ->
        let%bind i = traverse i in
        let%map o = traverse o in
        Arrow { i; o }
    | Id _ as x -> return x
    | Applicative { ty; arg } ->
        let%bind ty = traverse ty in
        let%map arg = traverse arg in
        Applicative { ty; arg }
    | TupleTy typ ->
        let%map x init = List.fold_map ~init ~f:(Fn.flip traverse) typ in
        TupleTy x
    | Var v -> (
        match Hashtbl.find imap v with
        | Some v -> return v
        | None ->
            let%map cursor = Type_map.bind_var in
            Hashtbl.set imap ~key:v ~data:cursor;
            cursor)
  in
  traverse ty
