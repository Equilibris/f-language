open Core
open Irs.Ast
open Core.Option.Let_syntax

let rec replace_var var_map ty =
  let nest = replace_var var_map in
  match ty with
  | Arrow { i; o } -> Arrow { i = nest i; o = nest o }
  | Applicative { ty; arg } -> Applicative { ty = nest ty; arg = nest arg }
  | TupleTy v -> TupleTy (List.map ~f:nest v)
  | Var lookup as default ->
      Map.find var_map lookup
      |> Option.value_or_thunk ~default:(fun () -> default)
  | Id _ as v -> v

let rec replace_var_deep visited var_map ty =
  match ty with
  | Arrow { i; o } ->
      let%bind i = replace_var_deep visited var_map i in
      let%map o = replace_var_deep visited var_map o in
      Arrow { i; o }
  | Applicative { ty; arg } ->
      let%bind ty = replace_var_deep visited var_map ty in
      let%map arg = replace_var_deep visited var_map arg in
      Applicative { ty; arg }
  | TupleTy v ->
      let%map ls = Ds.List.map_opt ~f:(replace_var_deep visited var_map) v in
      TupleTy ls
  | Var lookup as default -> (
      if (* Recursion detected *)
         Set.mem visited lookup then None
      else
        match Map.find var_map lookup with
        | None -> Some default
        | Some v ->
            let visited = Set.add visited lookup in
            replace_var_deep visited var_map v)
  | Id _ as v -> Some v

(* Identity 'a, Associative and commutative up to a-conversion *)
let rec unify eq_t var_map a b =
  match (a, b) with
  (* composite cases *)
  | Arrow { i; o }, Arrow { i = i2; o = o2 } ->
      let%bind var_map, i = unify eq_t var_map i i2 in
      let o = replace_var var_map o in
      let o2 = replace_var var_map o2 in
      let%map var_map, o = unify eq_t var_map o o2 in
      (var_map, Arrow { i; o })
  | Applicative { ty; arg }, Applicative { ty = ty2; arg = arg2 } ->
      let%bind var_map, ty = unify eq_t var_map ty ty2 in
      let arg = replace_var var_map arg in
      let arg2 = replace_var var_map arg2 in
      let%map var_map, arg = unify eq_t var_map arg arg2 in
      (var_map, Applicative { arg; ty })
  | TupleTy tup, TupleTy tup2 ->
      let tup, rem = List.zip_with_remainder tup tup2 in
      if Option.is_some rem then None
      else
        let%map var_map, tup =
          List.fold
            ~init:(Some (var_map, []))
            ~f:(fun last (a, b) ->
              let%bind var_map, last = last in
              let a = replace_var var_map a in
              let b = replace_var var_map b in
              let%map var_map, v = unify eq_t var_map a b in
              (var_map, v :: last))
            tup
        in
        (var_map, TupleTy (List.map ~f:(replace_var var_map) tup))
  (* Also handles case where both are variables *)
  | Var v, x | x, Var v ->
      let deep = Map.set var_map ~key:v ~data:x in
      Some (deep, x)
  | a, b -> if equal_ty eq_t a b then Some (var_map, a) else None
(* Error can be deduced*)
