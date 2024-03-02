open Core
open Irs
open Core.Option.Let_syntax
open Infer_expr
open Unify

let gather_top_level flat_ir id =
  let ty_map = Type_map.empty (module Int) in
  let%bind flat_ir, ty_map, _ = gather_type flat_ir ty_map id in
  let l = Ds.Index_set.ls ty_map.stack in

  let rec munch flat_ir = function
    | [] ->
        let%map v = Map.find (Flat.fn_ty_map flat_ir) id in
        (flat_ir, ty_map, v)
    | h :: tl -> (
        match
          ( Map.find (Type_map.id_ty_map ty_map) h,
            Map.find (Flat.fn_ty_map flat_ir) h )
        with
        | Some update, Some curr ->
            (* We ignore this continuation as it might use differing
               variables caused by if for example it is once canonicalized
               yet subsequently not. This might need a fix
            *)
            let%bind var_map, ty = unify Int.equal ty_map.var_map update curr in
            let%bind ty =
              replace_var_deep (Set.empty (module Int)) var_map ty
            in

            let flat_ir =
              {
                flat_ir with
                fn_ty_map = Map.set (Flat.fn_ty_map flat_ir) ~key:h ~data:ty;
              }
            in
            munch flat_ir tl
        (* TODO: reason about if this is correct *)
        | None, Some ty ->
            let%bind ty =
              replace_var_deep (Set.empty (module Int)) ty_map.var_map ty
            in
            let flat_ir =
              {
                flat_ir with
                fn_ty_map = Map.set (Flat.fn_ty_map flat_ir) ~key:h ~data:ty;
              }
            in
            munch flat_ir tl
        | _ -> munch flat_ir tl)
  in

  munch flat_ir l
