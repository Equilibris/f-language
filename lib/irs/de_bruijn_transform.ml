open Core
open Ast
open Ds
open State.Let_syntax

(** De Bruijn index Transformations *)

(*
 TODO:
 - [ ] Look into using binding overriding to make this code a bit nicer
 *)

(*
 Variable name index

 - If a var starts with e or t it is expr or type respectivley
 - i2s and s2i are int to string and v.v.
 - xreq is a requests system to manage possibly undefined variables
 *)

let scope_update x =
  State.update (fun current old -> Namespace.scope old current) x

let rec pat_name_mapper v =
  match v with
  | BindingPat v ->
      let%map v = Namespace.bind v in
      BindingPat v
  | TuplePat t ->
      let%map t ens = List.fold_map ~init:ens ~f:(Fn.flip pat_name_mapper) t in
      TuplePat t
  | ConstructorPat (name, value) ->
      let%bind name = Namespace.resolve name in
      let%map value = pat_name_mapper value in
      ConstructorPat (name, value)

let rec expr_name_mapper = function
  | Bind { name; value; within } ->
      let%bind value = expr_name_mapper value in
      let%bind old_ens = State.inspect in
      let%bind name = Namespace.bind name in
      let%bind within = expr_name_mapper within in
      let%map () = scope_update old_ens in
      Bind { name; value; within }
  | Match { scrutinee; arms } ->
      let%bind scrutinee = expr_name_mapper scrutinee in
      let%map arms ens =
        List.fold_map ~init:ens
          ~f:
            (Fn.flip (fun (pat, expr) ->
                 let%bind old_ns = State.inspect in
                 let%bind pat = pat_name_mapper pat in
                 let%bind expr = expr_name_mapper expr in
                 let%map () = scope_update old_ns in
                 (pat, expr)))
          arms
      in

      Match { scrutinee; arms }
  | Lambda { binding; content } ->
      let%bind old_ens = State.inspect in
      let%bind binding = Namespace.bind binding in
      let%bind content = expr_name_mapper content in
      let%map () = scope_update old_ens in
      Lambda { binding; content }
  | Constructor (name, value) ->
      let%bind name = Namespace.resolve name in
      let%map value = expr_name_mapper value in
      Constructor (name, value)
  | Call { callee; arg } ->
      let%bind callee = expr_name_mapper callee in
      let%map arg = expr_name_mapper arg in
      Call { callee; arg }
  | Tuple a ->
      let%map a ens = List.fold_map ~init:ens ~f:(Fn.flip expr_name_mapper) a in
      Tuple a
  | Id name ->
      let%map name = Namespace.resolve name in
      Expr.(Id name)

let rec ty_name_mapper = function
  | Var name ->
      let%map name = Namespace.resolve name in
      Var name
  | Id name ->
      let%map name = Namespace.resolve name in
      Id name
  | Applicative { ty; arg } ->
      let%bind ty = ty_name_mapper ty in
      let%map arg = ty_name_mapper arg in
      Applicative { ty; arg }
  | Arrow { i = input; o = output } ->
      let%bind input = ty_name_mapper input in
      let%map output = ty_name_mapper output in
      Arrow { i = input; o = output }
  | TupleTy tuple ->
      let%map a tns =
        List.fold_map ~init:tns ~f:(Fn.flip ty_name_mapper) tuple
      in
      TupleTy a

module S = struct
  type ('a, 'b, 'c) s = {
    ens : ('a, 'b, 'c) Namespace.t;
    tns : ('a, 'b, 'c) Namespace.t;
  }
end

open S

(* sad eta expantion noises *)
let set_ens x =
  State.translate (fun { ens; tns = _ } -> ens) (fun ens s -> { s with ens }) x

let set_tns x =
  State.translate (fun { ens = _; tns } -> tns) (fun tns s -> { s with tns }) x

let handle_decl { name; expr } =
  let%bind name = set_ens (Namespace.assign name) in
  let%bind old_ens = set_ens State.inspect in
  let%bind expr = set_ens (expr_name_mapper expr) in
  let%map () = set_ens (scope_update old_ens) in
  Decl { name; expr }

let handle_ty_def { name; vars; constructors } =
  let%bind name = set_tns (Namespace.assign name) in
  let%bind old_tns = set_tns State.inspect in
  let%bind vars =
    set_tns (fun tns ->
        List.fold_map ~init:tns ~f:(Fn.flip Namespace.bind) vars)
  in
  let%bind constructors init =
    List.fold_map ~init
      ~f:
        (Fn.flip (fun { constructor; ty } ->
             let%bind constructor = set_ens (Namespace.assign constructor) in
             let%map ty = set_tns (ty_name_mapper ty) in
             { constructor; ty }))
      constructors
  in
  let%map () = set_tns (scope_update old_tns) in
  TyDef { name; vars; constructors }

let top_level_name_mapper l =
  let%map x init =
    List.fold_map ~init
      ~f:
        (Fn.flip (fun curr ->
             let%map value =
               match curr with
               | Decl x -> handle_decl x
               | TyDef x -> handle_ty_def x
               | DeclTy _ -> State.return (failwith "todo")
             in
             value))
      l
  in
  x
