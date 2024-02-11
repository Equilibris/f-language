open! Core
open Ds
open Ast

type ('i_cmp, 'k_cmp, 'm_cmp) flat_ir = {
  tns : (string, 'i_cmp, 'k_cmp) Namespace.t;
  ens : (string, 'i_cmp, 'k_cmp) Namespace.t;
  ty_def_map : (int, int ty_def, 'm_cmp) Map.t;
  fn_def_map : (int, (int, int expr) decl_stmt, 'm_cmp) Map.t;
  fn_ty_map : (int, int ty, 'm_cmp) Map.t;
}
[@@deriving make, fields]

let make = make_flat_ir

let rec of_ens_tns_stream (ens, tns, stream) =
  match stream with
  | [] ->
      make ~ens ~tns
        ~ty_def_map:(Map.empty (module Int))
        ~fn_def_map:(Map.empty (module Int))
        ~fn_ty_map:(Map.empty (module Int))
  | Decl ({ name; expr = _ } as v) :: stream ->
      let stream = of_ens_tns_stream (ens, tns, stream) in
      { stream with fn_def_map = Map.set ~key:name ~data:v stream.fn_def_map }
  | TyDef ({ name; vars; constructors } as v) :: stream ->
      let stream = of_ens_tns_stream (ens, tns, stream) in
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
  | DeclTy _ :: _ -> failwith "todo"
