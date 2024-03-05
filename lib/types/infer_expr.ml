open Core
open Irs
open Irs.Ast
open Core.Option.Let_syntax
open Unify
open Canonicalize

(*
 TODO: if we reverse the arguments to the function we can
 do most of the work using currying and then a custom
 let binding that lets us recive a given input from a
 curried function to achive quasimutability
 *)

module State = struct
  type ('a, 'b, 'c) t = {
    flat_ir : ('a, 'b, 'c) Flat.flat_ir;
    ty_map : ('a, 'b) Type_map.t;
  }
end

(* open State *)

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

let ty_map_unify ty_map a b =
  let%map var_map, o = unify Int.equal (Type_map.var_map ty_map) a b in
  ({ ty_map with var_map }, o)

let rec app flat_ir ty_map fn_ty arg_ty =
  match fn_ty with
  | Arrow { i; o } ->
      let%map var_map, _ = unify Int.equal (Type_map.var_map ty_map) i arg_ty in
      (* replace_var only maps variable equality as types are strictly distinct *)
      let o = replace_var var_map o in
      (flat_ir, { ty_map with var_map }, o)
  | Var _ as v ->
      let ty_map, i = Type_map.bind_var ty_map in
      let ty_map, o = Type_map.bind_var ty_map in
      let i_type = Arrow { i; o } in
      let%bind ty_map, _ = ty_map_unify ty_map v i_type in

      app flat_ir ty_map i_type arg_ty
  | _ -> None

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
let rec gather_type flat_ir ty_map id =
  if Type_map.mem ty_map id then
    match Map.find ty_map.id_ty_map id with
    | None ->
        let ty_map, ty_var = Type_map.mint ty_map id in
        Some (flat_ir, ty_map, ty_var)
    | Some v -> Some (flat_ir, ty_map, v)
  else
    match Map.find (Type_map.id_ty_map ty_map) id with
    | Some v -> Some (flat_ir, ty_map, v)
    | None -> (
        let fn_ty_map = Flat.fn_ty_map flat_ir in
        match Map.find fn_ty_map id with
        | Some v -> Some (flat_ir, ty_map, v)
        | None ->
            let fn_def_map = Flat.fn_def_map flat_ir in
            (* Reference to undefined function, this should in theory not be
               possible if data is generated from the given function.
               Thereby I am ok using exn here *)
            let { name = _; expr } = Map.find_exn fn_def_map id in
            let ty_map = Type_map.enqueue ty_map id in
            let%map flat_ir, ty_map, ty =
              infer_expr flat_ir ty_map (Set.empty (module Int)) expr
            in
            ( Flat.
                {
                  flat_ir with
                  fn_ty_map = Map.set ~key:id ~data:ty (fn_ty_map flat_ir);
                },
              ty_map,
              ty ))

and infer_pat flat_ir ty_map nonfree = function
  | BindingPat v ->
      let ty_map, ty_var = Type_map.mint ty_map v in
      let nonfree = Set.add nonfree v in

      Some (flat_ir, ty_map, nonfree, ty_var)
  | ConstructorPat (a, b) ->
      let%bind flat_ir, ty_map, ty = gather_type flat_ir ty_map a in
      let ty_map, ty = canonicalize ty_map ty in
      let%bind { i; o } = match ty with Arrow v -> Some v | _ -> None in
      let%bind flat_ir, ty_map, nonfree, deep =
        infer_pat flat_ir ty_map nonfree b
      in
      let%bind ty_map, _ = ty_map_unify ty_map deep i in
      let%map ty_map =
        Set.to_list nonfree
        |> List.fold ~init:(Some ty_map) ~f:(fun ty_map other ->
               let%bind ty_map = ty_map in
               Type_map.replace_in_place ty_map other)
      in
      (flat_ir, ty_map, nonfree, o)
  | TuplePat x ->
      let%map flat_ir, ty_map, nonfree, o =
        List.fold
          ~init:(Some (flat_ir, ty_map, nonfree, []))
          ~f:(fun curr next ->
            let%bind flat_ir, ty_map, nonfree, last = curr in

            let%map flat_ir, ty_map, nonfree, next =
              infer_pat flat_ir ty_map nonfree next
            in

            (flat_ir, ty_map, nonfree, next :: last))
          x
      in
      (flat_ir, ty_map, nonfree, TupleTy (List.rev o))

(** Infers the type for a given expression it takes
    in 3 quasi-mutable params
    - flat_ir   The flat intermediate representation
                used for function lookup
    - ty_map    The type map, used for lookup of
                variables that do not escape the
                scope of the current function
                and also type variable equality
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
and infer_expr flat_ir ty_map nonfree =
  let open Type_map in
  function
  | Call { callee; arg } ->
      let%bind flat_ir, ty_map, callee =
        infer_expr flat_ir ty_map nonfree callee
      in
      let%bind flat_ir, ty_map, arg = infer_expr flat_ir ty_map nonfree arg in
      app flat_ir ty_map callee arg
  | Bind { name; value; within } ->
      let nonfree = Set.add nonfree name in
      let%bind flat_ir, ty_map, value =
        infer_expr flat_ir ty_map nonfree value
      in
      (* TODO: is this redundant *)
      let ty_map, _ = mint ty_map name in
      let%bind flat_ir, ty_map, within =
        infer_expr flat_ir ty_map nonfree within
      in
      let%map ty_map = set_or_update ty_map name value in
      let within = replace_var ty_map.var_map within in

      (flat_ir, ty_map, within)
  | Match { scrutinee; arms } ->
      let%bind flat_ir, ty_map, scrutinee_ty =
        infer_expr flat_ir ty_map nonfree scrutinee
      in
      let ty_map, arm_ty = Type_map.bind_var ty_map in

      let%map flat_ir, ty_map, _, arm_ty =
        (* let%bind _ = *)
        List.fold
          ~init:(Some (flat_ir, ty_map, scrutinee_ty, arm_ty))
          ~f:(fun last (pat, ex) ->
            let%bind flat_ir, ty_map, scrutinee_ty, arm_ty = last in
            let%bind flat_ir, ty_map, nonfree, u_ty =
              infer_pat flat_ir ty_map nonfree pat
            in

            let%bind ty_map, scrutinee_ty =
              ty_map_unify ty_map u_ty scrutinee_ty
            in
            let%bind flat_ir, ty_map, u_ty =
              infer_expr flat_ir ty_map nonfree ex
            in
            let%map ty_map, arm_ty = ty_map_unify ty_map u_ty arm_ty in

            (flat_ir, ty_map, scrutinee_ty, arm_ty))
          arms
      in
      (flat_ir, ty_map, arm_ty)
  | Lambda { binding; content } ->
      let nonfree = Set.add nonfree binding in
      let ty_map, _ = mint ty_map binding in
      let%bind flat_ir, ty_map, o = infer_expr flat_ir ty_map nonfree content in
      let%map flat_ir, ty_map, i = gather_type flat_ir ty_map binding in
      (flat_ir, ty_map, Arrow { i; o })
  | Constructor (constructor_e, arg_e) ->
      let%bind flat_ir, ty_map, constructor_ty =
        gather_type flat_ir ty_map constructor_e
      in
      let ty_map, constructor_ty = canonicalize ty_map constructor_ty in
      let%bind flat_ir, ty_map, arg_e =
        infer_expr flat_ir ty_map nonfree arg_e
      in
      app flat_ir ty_map constructor_ty arg_e
  | Tuple v ->
      let%map flat_ir, ty_map, ls =
        List.fold
          ~init:(Some (flat_ir, ty_map, []))
          ~f:(fun acc e ->
            let%bind flat_ir, ty_map, ls = acc in
            let%map flat_ir, ty_map, ty = infer_expr flat_ir ty_map nonfree e in
            (flat_ir, ty_map, ty :: ls))
          v
      in
      (flat_ir, ty_map, TupleTy (List.rev ls))
  | Id v ->
      let nonfree = Set.mem nonfree v in

      let%map flat_ir, ty_map, ty = gather_type flat_ir ty_map v in

      if nonfree then (flat_ir, ty_map, ty)
      else
        let ty_map, v = canonicalize ty_map ty in
        (flat_ir, ty_map, v)
