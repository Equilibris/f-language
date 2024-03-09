open Core
open Irs.Ast
open Core.Option.Let_syntax
open Unify

type ('a, 'cmp) t = {
  var_map : (int, int ty, 'a) Map.t;
  id_ty_map : (int, int ty, 'a) Map.t;
  cursor : int; [@default 0]
  vset : (int, 'cmp) Set.t;
}
[@@deriving make, fields]

module Setters (M : Base.Monad.S) = struct
  module M = Ds.State_t (M)

  let set_var_map x =
    M.translate
      (fun { var_map; id_ty_map = _; cursor = _; vset = _ } -> var_map)
      (fun var_map s -> { s with var_map })
      x

  let set_id_ty_map x =
    M.translate
      (fun { var_map = _; id_ty_map; cursor = _; vset = _ } -> id_ty_map)
      (fun id_ty_map s -> { s with id_ty_map })
      x
end

let empty m =
  make ~vset:(Set.empty m) ~var_map:(Map.empty m) ~id_ty_map:(Map.empty m) ()

let enqueue e v = { v with vset = Set.add v.vset e }

let set_or_update id ty ty_map =
  match Map.find ty_map.id_ty_map id with
  | None ->
      Some { ty_map with id_ty_map = Map.set ty_map.id_ty_map ~key:id ~data:ty }
  | Some v ->
      let%map var_map, ty = unify Int.equal v ty ty_map.var_map in
      {
        ty_map with
        var_map;
        id_ty_map = Map.set ty_map.id_ty_map ~key:id ~data:ty;
      }

let mem e v = Set.mem v.vset e

let bind_var ty_map =
  let cursor = cursor ty_map in
  ({ ty_map with cursor = cursor + 1 }, Var cursor)

let replace_in_place key ty_map =
  let%bind data = Map.find ty_map.id_ty_map key in
  let%map data = replace_var_deep data ty_map.var_map in
  { ty_map with id_ty_map = Map.set ty_map.id_ty_map ~key ~data }

let mint id ty_map =
  let ty_map, ty_var = bind_var ty_map in
  (* Since unification of a type variable and any other variable always
     unifies we can unpack its value as such. *)
  (set_or_update id ty_var ty_map |> Option.value_exn, ty_var)
