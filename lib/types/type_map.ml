open Core
open Irs.Ast
open Core.Option.Let_syntax
open Ds
open Unify

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

let mint v id =
  let v, ty_var = bind_var v in
  (* Since unification of a type variable and any other variable always
     unifies we can unpack its value as such. *)
  (set_or_update v id ty_var |> Option.value_exn, ty_var)
