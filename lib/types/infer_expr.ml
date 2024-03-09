open Core
open Irs
open Irs.Ast
open Unify
open Canonicalize
open Ds
open State_opt
open State_opt.Let_syntax
open Type_map.Setters (Option)
open Flat.Setters (Option)

(*
 TODO: if we reverse the arguments to the function we can
 do most of the work using currying and then a custom
 let binding that lets us recive a given input from a
 curried function to achive quasimutability
 *)

module State = struct
  type ('a, 'b, 'c, 'd, 'e) t = {
    flat_ir : ('a, 'b, 'c) Flat.flat_ir;
    ty_map : ('d, 'e) Type_map.t;
  }

  module Setters (M : Base.Monad.S) = struct
    module M = State_t (M)
    open M

    let set_ty_map x =
      translate
        (fun { flat_ir = _; ty_map } -> ty_map)
        (fun ty_map s -> { s with ty_map })
        x

    let set_flat_ir x =
      translate
        (fun { flat_ir; ty_map = _ } -> flat_ir)
        (fun flat_ir s -> { s with flat_ir })
        x
  end
end

open State
open State.Setters (Option)

let show_var_map ?(key_map = Int.to_string) v =
  Map.to_alist v
  |> List.map ~f:(fun (k, v) ->
         Printf.sprintf "\t%s = %s" (key_map k)
           (ty_to_src Int.to_string Int.to_string v))
  |> String.concat ~sep:",\n"

let print_ty_map ?(key_map = Int.to_string) ty_map =
  let () =
    show_var_map (Type_map.var_map ty_map) ~key_map:(Printf.sprintf "'%i")
    |> printf "var_map = {\n%s\n}\n"
  in
  show_var_map ~key_map (Type_map.id_ty_map ty_map)
  |> printf "id_ty_map = {\n%s\n}\n"

let print_ty ?(mapper = Int.to_string) name x =
  ty_to_src mapper Int.to_string x |> printf "%s = %s\n" name

let ty_map_unify a b =
  let%map o = unify Int.equal a b |> set_var_map in
  o

let rec app fn_ty arg_ty =
  match fn_ty with
  | Arrow { i; o } ->
      let%bind _ = ty_map_unify i arg_ty |> set_ty_map in
      (* replace_var only maps variable equality as types are strictly distinct *)
      let%map o =
        replace_var o |> i_ret |> effectless |> set_var_map |> set_ty_map
      in
      o
  | Var _ as v ->
      let%bind i = Type_map.bind_var |> i_ret |> set_ty_map in
      let%bind o = Type_map.bind_var |> i_ret |> set_ty_map in
      let i_type = Arrow { i; o } in
      let%bind _ = ty_map_unify v i_type |> set_ty_map in

      app i_type arg_ty
  | _ -> t_return None

(** gather_type does exactly what one thinks it does;
    it asks the current context for the type of a
    identifier, if this is not accessible then one
    calls the inference algorithm on it. In the case
    of a recursion it assumes the type is 'a and
    unifies later down the line.

    It takes similar params to infer_expr yet the
    final one is now an identifier rather than an
    expr.
 *)
let rec gather_type id =
  let%bind ty_map = inspect |> set_ty_map in
  let%bind condition = Type_map.mem id |> i_ret |> effectless |> set_ty_map in
  if condition then
    match Map.find ty_map.id_ty_map id with
    | None ->
        let%map ty_var = Type_map.mint id |> i_ret |> set_ty_map in
        ty_var
    | Some v -> return v
  else
    let%bind id_ty_map = inspect |> set_id_ty_map |> set_ty_map in
    match Map.find id_ty_map id with
    | Some v -> return v
    | None -> (
        let%bind fn_ty_map = inspect |> set_fn_ty_map |> set_flat_ir in
        match Map.find fn_ty_map id with
        | Some v -> return v
        | None ->
            let%bind fn_def_map = inspect |> set_fn_def_map |> set_flat_ir in
            (* Reference to undefined function, this should in theory not be
               possible if data is generated from the given function.
               Thereby I am ok using exn here *)
            let { name = _; expr } = Map.find_exn fn_def_map id in
            let%bind () = Type_map.enqueue id |> update |> set_ty_map in
            let%bind ty = infer_expr (Set.empty (module Int)) expr in
            let%map () =
              Map.set ~key:id ~data:ty |> update |> set_fn_ty_map |> set_flat_ir
            in

            ty)

and infer_pat nonfree v =
  match v with
  | BindingPat v ->
      let%map ty_var = Type_map.mint v |> i_ret |> set_ty_map in
      let nonfree = Set.add nonfree v in

      (nonfree, ty_var)
  | ConstructorPat (a, b) ->
      let%bind ty = gather_type a in
      let%bind ty = canonicalize ty |> i_ret |> set_ty_map in
      let%bind { i; o } =
        t_return (match ty with Arrow v -> Some v | _ -> None)
      in
      let%bind nonfree, deep = infer_pat nonfree b in
      let%bind _ = ty_map_unify deep i |> set_ty_map in
      let%map () =
        effect (fun ty_map ->
            Set.to_list nonfree
            |> List.fold ~init:(Some ty_map) ~f:(fun ty_map other ->
                   let open Option.Let_syntax in
                   let%bind ty_map = ty_map in
                   Type_map.replace_in_place other ty_map))
        |> set_ty_map
      in

      (nonfree, o)
  | TuplePat x ->
      let%map nonfree, rev_tuple =
        List.fold
          ~init:(fun state -> Some (state, (nonfree, [])))
          ~f:(fun current next ->
            (* TODO: This can be a fold_map *)
            let%bind nonfree, last = current in

            let%map nonfree, next = infer_pat nonfree next in
            (nonfree, next :: last))
          x
      in

      (nonfree, TupleTy (List.rev rev_tuple))

(** Infers the type for a given expression it takes
    in 1 quasi-mutable params
    - nonfree   The set of variables that are bound 
                within the current inference context.
                This includes stuff like all variables,
                function params and so on. This is used
                to figure out when to canonicalize a
                given value such that its variables do
                not cause collisions with local type
                variables.

    After all of these one finally passes the expression
    for inference. This is also the point of recursion.
  *)
and infer_expr nonfree v =
  let open! Type_map in
  match v with
  | Call { callee; arg } ->
      let%bind callee = infer_expr nonfree callee in
      let%bind arg = infer_expr nonfree arg in
      app callee arg
  | Bind { name; value; within } ->
      let nonfree = Set.add nonfree name in
      let%bind value = infer_expr nonfree value in
      (* TODO: is this redundant *)
      let%bind _ = mint name |> i_ret |> set_ty_map in
      let%bind within = infer_expr nonfree within in
      let%bind () = set_or_update name value |> effect |> set_ty_map in
      let%map within =
        replace_var within |> i_ret |> effectless |> set_var_map |> set_ty_map
      in

      within
  | Match { scrutinee; arms } ->
      let%bind scrutinee_ty = infer_expr nonfree scrutinee in
      let%bind arm_ty = Type_map.bind_var |> i_ret |> set_ty_map in

      let%map _, arm_ty =
       fun state ->
        List.fold
          ~init:(Some (state, (scrutinee_ty, arm_ty)))
          ~f:(fun last (pat, ex) ->
            let open Option.Let_syntax in
            let%bind { flat_ir; ty_map }, (scrutinee_ty, arm_ty) = last in
            let%bind { flat_ir; ty_map }, (nonfree, u_ty) =
              infer_pat nonfree pat { flat_ir; ty_map }
            in

            let%bind ty_map, scrutinee_ty =
              ty_map_unify u_ty scrutinee_ty ty_map
            in
            let%bind { flat_ir; ty_map }, u_ty =
              infer_expr nonfree ex { flat_ir; ty_map }
            in
            let%map ty_map, arm_ty = ty_map_unify u_ty arm_ty ty_map in

            ({ flat_ir; ty_map }, (scrutinee_ty, arm_ty)))
          arms
      in
      arm_ty
  | Lambda { binding; content } ->
      let nonfree = Set.add nonfree binding in
      let%bind _ = mint binding |> i_ret |> set_ty_map in
      let%bind o = infer_expr nonfree content in
      let%map i = gather_type binding in
      Arrow { i; o }
  | Constructor (constructor_e, arg_e) ->
      let%bind constructor_ty = gather_type constructor_e in
      let%bind constructor_ty =
        canonicalize constructor_ty |> i_ret |> set_ty_map
      in
      let%bind arg_e = infer_expr nonfree arg_e in
      app constructor_ty arg_e
  | Tuple v ->
      (* TODO: make fold_map monad *)
      let%map ls =
        List.fold
          ~init:(fun state -> Some (state, []))
          ~f:(fun acc e ->
            let%bind ls = acc in
            let%map ty = infer_expr nonfree e in
            ty :: ls)
          v
      in
      TupleTy (List.rev ls)
  | Id v ->
      let nonfree = Set.mem nonfree v in

      let%bind ty = gather_type v in

      if nonfree then return ty
      else
        let%map v = canonicalize ty |> i_ret |> set_ty_map in
        v
