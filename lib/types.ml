open Core
open Irs
open Irs.Ast
open Core.Option.Let_syntax

let rec replace_var sr_map ty =
  let nest = replace_var sr_map in
  match ty with
  | Arrow { i; o } -> Arrow { i = nest i; o = nest o }
  | Applicative { ty; arg } -> Applicative { ty = nest ty; arg = nest arg }
  | TupleTy v -> TupleTy (List.map ~f:nest v)
  | Var lookup as default ->
      Map.find sr_map lookup
      |> Option.value_or_thunk ~default:(fun () -> default)
  | Id _ as v -> v

(* Identity 'a, Associative and commutative up to a-conversion *)
let rec unify eq_t sr_map a b =
  match (a, b) with
  (* composite cases *)
  | Arrow { i; o }, Arrow { i = i2; o = o2 } ->
      let%bind sr_map, i = unify eq_t sr_map i i2 in
      let o = replace_var sr_map o in
      let o2 = replace_var sr_map o2 in
      let%map sr_map, o = unify eq_t sr_map o o2 in
      (sr_map, Arrow { i; o })
  | Applicative { ty; arg }, Applicative { ty = ty2; arg = arg2 } ->
      let%bind sr_map, ty = unify eq_t sr_map ty ty2 in
      let arg = replace_var sr_map arg in
      let arg2 = replace_var sr_map arg2 in
      let%map sr_map, arg = unify eq_t sr_map arg arg2 in
      (sr_map, Applicative { arg; ty })
  | TupleTy tup, TupleTy tup2 ->
      let tup, rem = List.zip_with_remainder tup tup2 in
      if Option.is_some rem then None
      else
        let%map sr_map, tup =
          List.fold
            ~init:(Some (sr_map, []))
            ~f:(fun last (a, b) ->
              let%bind sr_map, last = last in
              let a = replace_var sr_map a in
              let b = replace_var sr_map b in
              let%map sr_map, v = unify eq_t sr_map a b in
              (sr_map, v :: last))
            tup
        in
        (sr_map, TupleTy (List.rev_map ~f:(replace_var sr_map) tup))
  (* Also handles case where both are variables *)
  | Var v, x | x, Var v ->
      let deep = Map.set sr_map ~key:v ~data:x in
      Some (deep, x)
  | a, b -> if equal_ty eq_t a b then Some (sr_map, a) else None
(* Error can be deduced*)

module TypeMap = struct
  open Ds

  type ('a, 'cmp) t = {
    var_map : (int, int ty, 'a) Map.t;
    id_ty_map : (int, int ty, 'a) Map.t;
    cursor : int; [@default 0]
    stack : (int, 'cmp) Index_set.t;
  }
  [@@deriving make, fields]

  let empty m =
    make ~stack:(Index_set.empty m) ~var_map:(Map.empty m)
      ~id_ty_map:(Map.empty m) ()

  let enqueue v e = { v with stack = Index_set.enqueue v.stack e }

  let set_or_update x id ty =
    match Map.find x.id_ty_map id with
    | None -> Some { x with id_ty_map = Map.set x.id_ty_map ~key:id ~data:ty }
    | Some v ->
        let%map var_map, ty = unify Int.equal x.var_map v ty in
        { x with var_map; id_ty_map = Map.set x.id_ty_map ~key:id ~data:ty }

  let pop v =
    let n, stack = Index_set.pop v.stack in
    (n, { v with stack })

  let mem v e = Index_set.mem v.stack e

  let bind_var v =
    let cursor = cursor v in
    ({ v with cursor = cursor + 1 }, Var cursor)
end

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
          List.fold ~init:(ty_map, [])
            ~f:(fun (ty_map, last) nx ->
              let ty_map, nx = traverse ty_map nx in
              (ty_map, nx :: last))
            typ
        in
        (ty_map, TupleTy x)
    | Var v -> (
        match Hashtbl.find imap v with
        | Some v -> (ty_map, v)
        | None ->
            let ty_map, cursor = TypeMap.bind_var ty_map in
            Hashtbl.set imap ~key:v ~data:cursor;
            (ty_map, cursor))
  in
  traverse ty_map ty

let app flat_ir ty_map t1 t2 =
  match t1 with
  | Arrow { i; o } ->
      let%map var_map, _ = unify Int.equal (TypeMap.var_map ty_map) i t2 in
      (* replace_var only maps variable equality as types are strictly distinct *)
      let o = replace_var var_map o in
      (flat_ir, { ty_map with var_map }, o)
  | _ -> None

let rec gather_type flat_ir ty_map id =
  if TypeMap.mem ty_map id then
    failwith "TODO: recursion (or mutual recursion) detected"
  else
    match Map.find (TypeMap.id_ty_map ty_map) id with
    | Some v -> Some (flat_ir, ty_map, v)
    | None -> (
        let fn_ty_map = Flat.fn_ty_map flat_ir in
        match Map.find fn_ty_map id with
        | Some v -> Some (flat_ir, ty_map, v)
        | None ->
            let fn_def_map = Flat.fn_def_map flat_ir in
            (* Reference to undefined function, this should in theory not be
               possible if data is generated from the given function.
               Thereby I am ok using exn here *)
            let { name = _; expr } = Map.find_exn fn_def_map id in
            let ty_map = TypeMap.enqueue ty_map id in
            let%map flat_ir, ty_map, ty =
              type_of_expr flat_ir ty_map (Set.empty (module Int)) expr
            in
            ( Flat.
                {
                  flat_ir with
                  fn_ty_map = Map.set ~key:id ~data:ty (fn_ty_map flat_ir);
                },
              ty_map,
              ty ))

and type_of_expr flat_ir ty_map nonfree =
  let open TypeMap in
  function
  | Call { callee; arg } ->
      let%bind flat_ir, ty_map, callee =
        type_of_expr flat_ir ty_map nonfree callee
      in
      let%bind flat_ir, ty_map, arg = type_of_expr flat_ir ty_map nonfree arg in
      app flat_ir ty_map callee arg
  | Bind { name; value; within } ->
      let nonfree = Set.add nonfree name in
      let%bind flat_ir, ty_map, value =
        type_of_expr flat_ir ty_map nonfree value
      in
      let ty_map, ty_var = bind_var ty_map in
      let%bind ty_map = set_or_update ty_map name ty_var in
      let%bind flat_ir, ty_map, within =
        type_of_expr flat_ir ty_map nonfree within
      in
      let%map ty_map = set_or_update ty_map name value in
      let within = replace_var ty_map.var_map within in

      (flat_ir, ty_map, within)
  | Match _ -> failwith "TODO: Match not encoutered"
  | Lambda { binding; content } ->
      let nonfree = Set.add nonfree binding in
      let ty_map, ty_var = bind_var ty_map in
      let%bind ty_map = set_or_update ty_map binding ty_var in
      let%bind flat_ir, ty_map, o =
        type_of_expr flat_ir ty_map nonfree content
      in
      let%map flat_ir, ty_map, i = gather_type flat_ir ty_map binding in
      (flat_ir, ty_map, Arrow { i; o })
  | Constructor (constructor_e, arg_e) ->
      let%bind flat_ir, ty_map, constructor_ty =
        gather_type flat_ir ty_map constructor_e
      in
      let%bind flat_ir, ty_map, arg_e =
        type_of_expr flat_ir ty_map nonfree arg_e
      in
      app flat_ir ty_map constructor_ty arg_e
  | Tuple v ->
      let%map flat_ir, ty_map, ls =
        List.fold
          ~init:(Some (flat_ir, ty_map, []))
          ~f:(fun acc e ->
            let%bind flat_ir, ty_map, ls = acc in
            let%map flat_ir, ty_map, ty =
              type_of_expr flat_ir ty_map nonfree e
            in
            (flat_ir, ty_map, ty :: ls))
          v
      in
      (flat_ir, ty_map, TupleTy (List.rev ls))
  | Id v ->
      let nonfree = Set.mem nonfree v in
      let%map flat_ir, ty_map, v = gather_type flat_ir ty_map v in

      if nonfree then (flat_ir, ty_map, v)
      else
        let ty_map, v = canonicalize ty_map v in
        (flat_ir, ty_map, v)

module Tests = struct
  open Core.Poly

  let unify a b =
    let%map _, t = unify Int.equal (Map.empty (module Int)) a b in
    t

  let%test _ =
    unify
      (TupleTy [ Var 0; Var 0; Var 1 ])
      (TupleTy [ Var 2; TupleTy []; Var 3 ])
    = Some (TupleTy [ TupleTy []; TupleTy []; Var 3 ])

  let%test _ = unify (TupleTy []) (Var 0) = Some (TupleTy [])
  let%test _ = unify (Var 0) (TupleTy []) = Some (TupleTy [])

  let%test _ =
    unify (Arrow { i = Var 0; o = Var 0 }) (Arrow { i = Id 0; o = Id 0 })
    = Some (Arrow { i = Id 0; o = Id 0 })

  (* let%test _ = unify (TupleTy []); *)

  (* let%test_unit _ = *)
  (*   unify (Arrow { i = Var 0; o = Var 0 }) (Arrow { i = Id 0; o = Id 0 }) *)
  (*   |> Option.value_exn *)
  (*   |> show_ty Format.pp_print_int *)
  (*   |> print_endline *)
end
