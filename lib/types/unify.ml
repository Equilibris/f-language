open Core
open Irs.Ast
open Ds.State_opt
open Core.Option.Let_syntax

let rec replace_var ty var_map =
  let nest x = replace_var x var_map in
  match ty with
  | Arrow { i; o } -> Arrow { i = nest i; o = nest o }
  | Applicative { ty; arg } -> Applicative { ty = nest ty; arg = nest arg }
  | TupleTy v -> TupleTy (List.map ~f:nest v)
  | Var lookup as default ->
      Map.find var_map lookup
      |> Option.value_or_thunk ~default:(fun () -> default)
  | Id _ as v -> v

let eset = Set.empty (module Int)

let rec replace_var_deep ?(visited = eset) ty var_map =
  match ty with
  | Arrow { i; o } ->
      let%bind i = replace_var_deep ~visited i var_map in
      let%map o = replace_var_deep ~visited o var_map in
      Arrow { i; o }
  | Applicative { ty; arg } ->
      let%bind ty = replace_var_deep ~visited ty var_map in
      let%map arg = replace_var_deep ~visited arg var_map in
      Applicative { ty; arg }
  | TupleTy v ->
      let%map ls =
        List.map ~f:(fun x -> replace_var_deep ~visited x var_map) v
        |> Option.all
      in
      TupleTy ls
  | Var lookup as default -> (
      if (* Recursion detected *)
         Set.mem visited lookup then None
      else
        match Map.find var_map lookup with
        | None -> Some default
        | Some v ->
            let visited = Set.add visited lookup in
            replace_var_deep ~visited v var_map)
  | Id _ as v -> Some v

(* Identity 'a, Associative and commutative up to a-conversion *)
let rec unify eq_t a b =
  let open Ds.State_opt.Let_syntax in
  match (a, b) with
  (* composite cases *)
  | Arrow { i; o }, Arrow { i = i2; o = o2 } ->
      let%bind i = unify eq_t i i2 in
      let%bind o = replace_var o |> i_ret |> effectless in
      let%bind o2 = replace_var o2 |> i_ret |> effectless in
      let%map o = unify eq_t o o2 in
      Arrow { i; o }
  | Applicative { ty; arg }, Applicative { ty = ty2; arg = arg2 } ->
      let%bind ty = unify eq_t ty ty2 in
      let%bind arg = replace_var arg |> i_ret |> effectless in
      let%bind arg2 = replace_var arg2 |> i_ret |> effectless in
      let%map arg = unify eq_t arg arg2 in
      Applicative { arg; ty }
  | TupleTy tup, TupleTy tup2 ->
      let tup, rem = List.zip_with_remainder tup tup2 in
      if Option.is_some rem then t_ret None
      else
        let%bind tup var_map =
          List.fold
            ~init:(Some (var_map, []))
            ~f:(fun last (a, b) ->
              let open Option.Let_syntax in
              let%bind var_map, last = last in
              let a = replace_var a var_map in
              let b = replace_var b var_map in
              let%map var_map, v = unify eq_t a b var_map in
              (var_map, v :: last))
            tup
        in
        let%map var_map = inspect in
        TupleTy (List.rev_map ~f:(Fn.flip replace_var var_map) tup)
  (* (* Also handles case where both are variables *) *)
  | Var v, x | x, Var v -> (
      (* The bug here is set can overwrite, we need to handle this case
                explicitly to ensure that the type is maintained. *)
      let%bind var_map = inspect in
      match Map.find var_map v with
      | Some curr_ty ->
          let%bind u_ty = unify eq_t curr_ty x in
          let%map () = update (Map.set ~key:v ~data:u_ty) in
          x
      | None ->
          let%map () = update (Map.set ~key:v ~data:x) in
          x)
  | a, b -> t_ret (if equal_ty eq_t a b then Some a else None)
(* Error can be deduced*)
