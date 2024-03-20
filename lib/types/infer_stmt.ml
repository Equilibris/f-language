open Core
open Irs
open Ds.State_opt
open Ds.State_opt.Let_syntax
open Unify
open Infer_expr
open Infer_expr.State
open Infer_expr.State.Setters (Option)
open Type_map.Setters (Option)
open Flat.Setters (Option)

let gather_top_level id =
  let%bind _ = gather_type id in
  let%bind ty_map = inspect |> set_ty_map in
  let l = Set.to_list ty_map.vset in
  let%bind _ =
    List.fold ~init:id_state
      ~f:(fun state key ->
        let%bind () = state in
        let%bind { flat_ir; ty_map } = inspect in

        match
          ( Map.find (Type_map.id_ty_map ty_map) key,
            Map.find (Flat.fn_ty_map flat_ir) key )
        with
        | Some new_ty, Some curr ->
            (* We ignore this continuation as it might use differing
               variables caused by if for example it is once canonicalized
               yet subsequently not. This might need a fix
            *)
            let%bind ty = ty_map_unify new_ty curr |> set_ty_map in
            let%bind ty =
              replace_var_deep ty |> effectless |> set_var_map |> set_ty_map
            in

            let%bind () =
              Map.set ~key ~data:ty |> update |> set_fn_ty_map |> set_flat_ir
            in
            return ()
        | None, Some ty ->
            let%bind ty =
              replace_var_deep ty |> effectless |> set_var_map |> set_ty_map
            in

            let%bind () =
              Map.set ~key ~data:ty |> update |> set_fn_ty_map |> set_flat_ir
            in
            return ()
        | _ -> return ())
      l
  in

  Fn.flip Map.find id |> effectless |> set_fn_ty_map |> set_flat_ir

let gather_top_level flat_ir id =
  let ty_map = Type_map.empty (module Int) in
  gather_top_level id { flat_ir; ty_map }
