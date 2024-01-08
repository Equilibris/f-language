open Core
open Ast
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

let rec unify sr_map a b =
  match (a, b) with
  (* composite cases *)
  | Arrow { i; o }, Arrow { i = i2; o = o2 } ->
      let%bind sr_map, i = unify sr_map i i2 in
      let o = replace_var sr_map o in
      let o2 = replace_var sr_map o2 in
      let%map sr_map, o = unify sr_map o o2 in
      (sr_map, Arrow { i; o })
  | Applicative { ty; arg }, Applicative { ty = ty2; arg = arg2 } ->
      let%bind sr_map, ty = unify sr_map ty ty2 in
      let arg = replace_var sr_map arg in
      let arg2 = replace_var sr_map arg2 in
      let%map sr_map, arg = unify sr_map arg arg2 in
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
              let%map sr_map, v = unify sr_map a b in
              (sr_map, v :: last))
            tup
        in
        (sr_map, TupleTy (List.rev_map ~f:(replace_var sr_map) tup))
  (* Also handles case where both are variables *)
  | Var v, x | x, Var v ->
      let deep = Map.set sr_map ~key:v ~data:x in
      Some (deep, x)
  | _, _ -> None (* Error can be deduced*)

module Tests = struct
  open Core.Poly

  let unify a b =
    let%map _, t = unify (Map.empty (module Int)) a b in
    t

  let%test _ =
    unify
      (TupleTy [ Var 0; Var 0; Var 1 ])
      (TupleTy [ Var 2; TupleTy []; Var 3 ])
    = Some (TupleTy [ TupleTy []; TupleTy []; Var 3 ])

  let%test _ = unify (TupleTy []) (Var 0) = Some (TupleTy [])
  let%test _ = unify (Var 0) (TupleTy []) = Some (TupleTy [])

  (* let%test _ = *)
  (*   unify (Arrow { i = Var 0; o = Var 0 }) (Arrow { i = Id 0; o = Id 0 }) *)
  (*   = Some (Arrow { i = Id 0; o = Id 0 }) *)

  let%test_unit _ =
    unify (Arrow { i = Var 0; o = Var 0 }) (Arrow { i = Id 0; o = Id 0 })
    |> Option.value_exn
    |> show_ty Format.pp_print_int
    |> print_endline
end
