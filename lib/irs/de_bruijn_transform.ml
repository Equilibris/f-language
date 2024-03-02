open Core
open Ast

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

open Ds.Namespace

let rec pat_name_mapper ens = function
  | BindingPat v ->
      let ens, v = bind ens v in
      (ens, BindingPat v)
  | TuplePat t ->
      let ens, t =
        List.fold_map ~init:ens
          ~f:(fun ens v ->
            let ens, v = pat_name_mapper ens v in
            (ens, v))
          t
      in
      (ens, TuplePat t)
  | ConstructorPat (name, value) ->
      let ens, name = resolve ens name in
      let ens, value = pat_name_mapper ens value in
      (ens, ConstructorPat (name, value))

let rec expr_name_mapper ens = function
  | Bind { name; value; within } ->
      let ens, value = expr_name_mapper ens value in
      let+ ens = ens in
      let ens, name = bind ens name in
      let ens, within = expr_name_mapper ens within in
      (ens, Bind { name; value; within })
  | Match { scrutinee; arms } ->
      let ens, scrutinee = expr_name_mapper ens scrutinee in
      let ens, arms =
        List.fold_map ~init:ens
          ~f:(fun ns (pat, expr) ->
            let+ ns = ns in
            let ns, pat = pat_name_mapper ns pat in
            let ns, expr = expr_name_mapper ns expr in
            (ns, (pat, expr)))
          arms
      in
      (ens, Match { scrutinee; arms })
  | Lambda { binding; content } ->
      let+ ens = ens in
      let ens, binding = bind ens binding in
      let ens, content = expr_name_mapper ens content in
      (ens, Lambda { binding; content })
  | Constructor (name, value) ->
      let ens, name = resolve ens name in
      let ens, value = expr_name_mapper ens value in
      (ens, Constructor (name, value))
  | Call { callee; arg } ->
      let ens, callee = expr_name_mapper ens callee in
      let ens, arg = expr_name_mapper ens arg in
      (ens, Call { callee; arg })
  | Tuple a ->
      let ens, a =
        List.fold_map ~init:ens
          ~f:(fun ens v ->
            let ens, v = expr_name_mapper ens v in
            (ens, v))
          a
      in
      (ens, Tuple a)
  | Id name ->
      let ens, name = resolve ens name in
      (ens, Id name)

let rec ty_name_mapper tns = function
  | Var name ->
      let tns, name = resolve tns name in
      (tns, Var name)
  | Id name ->
      let tns, name = resolve tns name in
      (tns, Id name)
  | Applicative { ty; arg } ->
      let tns, ty = ty_name_mapper tns ty in
      let tns, arg = ty_name_mapper tns arg in
      (tns, Applicative { ty; arg })
  | Arrow { i = input; o = output } ->
      let tns, input = ty_name_mapper tns input in
      let tns, output = ty_name_mapper tns output in
      (tns, Arrow { i = input; o = output })
  | TupleTy tuple ->
      let tns, a =
        List.fold_map ~init:tns
          ~f:(fun tns v ->
            let tns, v = ty_name_mapper tns v in
            (tns, v))
          tuple
      in
      (tns, TupleTy a)

let rec top_level_name_mapper ~ens ~tns = function
  | curr :: next ->
      let ens, tns, value =
        match curr with
        | Decl { name; expr } ->
            let ens, name = assign ens name in
            let nens, expr = expr_name_mapper ens expr in
            (scope ens nens, tns, Decl { name; expr })
        | TyDef { name; vars; constructors } ->
            let tns, name = assign tns name in
            let ntns, vars =
              List.fold ~init:(tns, [])
                ~f:(fun (tns, vars) var ->
                  let ns, var = bind tns var in
                  (ns, var :: vars))
                vars
            in
            let (ens, ntns), constructors =
              List.fold_map ~init:(ens, ntns)
                ~f:(fun (ens, tns) { constructor; ty } ->
                  let ens, constructor = assign ens constructor in
                  let tns, ty = ty_name_mapper tns ty in
                  ((ens, tns), { constructor; ty }))
                constructors
            in
            ( ens,
              scope tns ntns,
              (* TODO: Vars might be backwards *)
              TyDef { name; vars; constructors } )
        | DeclTy _ -> (ens, tns, failwith "todo")
      in
      let ens, tns, next = top_level_name_mapper ~ens ~tns next in
      (ens, tns, value :: next)
  | [] -> (ens, tns, [])
