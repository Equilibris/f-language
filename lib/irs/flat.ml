open! Core
open Ds
open Ast
open De_bruijn_transform.S

type ('i_cmp, 'k_cmp, 'm_cmp) flat_ir = {
  tns : (string, 'i_cmp, 'k_cmp) Namespace.t;
  ens : (string, 'i_cmp, 'k_cmp) Namespace.t;
  ty_def_map : (int, int ty_def, 'm_cmp) Map.t;
  fn_def_map : (int, int expr, 'm_cmp) Map.t;
  fn_ty_map : (int, int ty, 'm_cmp) Map.t;
}
[@@deriving make, fields]

module Setters (M : Base.Monad.S) = struct
  module M = Ds.State_t (M)

  let set_tns x =
    M.translate
      (fun { tns; ens = _; ty_def_map = _; fn_def_map = _; fn_ty_map = _ } ->
        tns)
      (fun tns s -> { s with tns })
      x

  let set_ens x =
    M.translate
      (fun { tns = _; ens; ty_def_map = _; fn_def_map = _; fn_ty_map = _ } ->
        ens)
      (fun ens s -> { s with ens })
      x

  let set_ty_def_map x =
    M.translate
      (fun { tns = _; ens = _; ty_def_map; fn_def_map = _; fn_ty_map = _ } ->
        ty_def_map)
      (fun ty_def_map s -> { s with ty_def_map })
      x

  let set_fn_def_map x =
    M.translate
      (fun { tns = _; ens = _; ty_def_map = _; fn_def_map; fn_ty_map = _ } ->
        fn_def_map)
      (fun fn_def_map s -> { s with fn_def_map })
      x

  let set_fn_ty_map x =
    M.translate
      (fun { tns = _; ens = _; ty_def_map = _; fn_def_map = _; fn_ty_map } ->
        fn_ty_map)
      (fun fn_ty_map s -> { s with fn_ty_map })
      x
end

let make = make_flat_ir

let of_ens_tns_stream (({ ens; tns } : ('a, 'b, 'c) s), stream) =
  List.fold
    ~init:
      (make ~ens ~tns
         ~ty_def_map:(Map.empty (module Int))
         ~fn_def_map:(Map.empty (module Int))
         ~fn_ty_map:(Map.empty (module Int)))
    ~f:(fun stream -> function
      | Decl ({ name; expr = _ } as v) ->
          {
            stream with
            fn_def_map = Map.set ~key:name ~data:v.expr stream.fn_def_map;
          }
      | TyDef ({ name; vars; constructors } as v) ->
          let ret_ty =
            List.fold ~init:(Id name)
              ~f:(fun ty arg -> Applicative { ty; arg = Var arg })
              vars
          in
          let entires =
            List.map
              ~f:(fun { constructor; ty } ->
                (constructor, Arrow { i = ty; o = ret_ty }))
              constructors
          in
          {
            stream with
            ty_def_map = Map.set ~key:name ~data:v stream.ty_def_map;
            fn_ty_map =
              List.fold ~init:stream.fn_ty_map
                ~f:(fun prev (name, ty) -> Map.set ~key:name ~data:ty prev)
                entires;
          }
      | DeclTy _ -> failwith "todo")
    stream
