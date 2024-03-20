open Core
open Ds.State_opt
open Ds.State_opt.Let_syntax
open Irs.Flat.Setters (Option)
open Irs.Ast
open Types

(** given a list of lists of length n, return a list of lists where we've
    flattened this. Doesn't acc have to be a correct transpose as all it has
    to do is swap columns and rows *)
let transpose x =
  let init =
    List.map ~f:(Fn.const []) @@ Option.value ~default:[] @@ List.hd x
  in
  (* We can use exn here as two tuples of differing lengths do not unify*)
  List.fold x ~init ~f:(Fn.flip (List.map2_exn ~f:List.cons))

exception Would_not_unify

let get_applicative_constructors ty =
  (* Might be reversed *)
  let rec get_args { ty; arg } =
    match ty with
    | Id v -> (v, [])
    | Applicative v ->
        let top, vals = get_args v in
        (top, arg :: vals)
    | _ -> raise Would_not_unify
  in
  let ty, args = get_args ty in
  let%map { name = _; vars; constructors } =
    Fn.flip Map.find ty |> effectless |> set_ty_def_map
  in
  let var_map = List.zip_exn vars args |> Map.of_alist_exn (module Int) in

  List.map
    ~f:(fun { constructor; ty } ->
      {
        constructor;
        ty =
          (* Why is this not deep? there exists no type a = ... a if
             this were to be allowed things would go wrong here *)
          replace_var ty var_map;
      })
    constructors

let rec handle_constructors constructors pats =
  let tbl = Hashtbl.create (module Int) in
  let types = Hashtbl.create (module Int) in
  List.iter
    ~f:(fun { constructor = key; ty } ->
      Hashtbl.set tbl ~key ~data:[];
      Hashtbl.set types ~key ~data:ty)
    constructors;

  List.iter
    ~f:(function
      | ConstructorPat (id, pat) ->
          Hashtbl.change tbl id ~f:(Option.map ~f:(List.cons pat))
      | _ -> raise Would_not_unify)
    pats;

  let%map handled_constructors =
    Hashtbl.to_alist tbl
    |> List.fold ~init:(return []) ~f:(fun state (constructor_id, pats) ->
           let%bind last = state in
           let ty = Hashtbl.find_exn types constructor_id in
           let%map vs = pattern_total ty pats in
           Option.map vs ~f:(fun v -> (constructor_id, v)) :: last)
  in

  match List.find_map handled_constructors ~f:Fn.id with
  | None -> None
  | Some (constructor_id, expr) -> Some (Constructor (constructor_id, expr))

(**
   Determines if a given family of patterns is total or not. Here are the
   meaning of some of the return_types:

   - None       -> Failure
   - Some(None) -> Success, there exists no value that this pattern
                   does not catch
   - Some(ls)   -> Success, the compiler detected that all the patterns
                   in ls are unhandled
 *)
and pattern_total ty pats =
  if List.exists ~f:(function BindingPat _ -> true | _ -> false) pats then
    return None
  else if List.is_empty pats then return (Some (Expr.Id 0))
  else
    match ty with
    | Applicative _ -> fail
    | Id ty ->
        let%bind { name = _; vars = _; constructors } =
          Fn.flip Map.find ty |> effectless |> set_ty_def_map
        in
        handle_constructors constructors pats
    | TupleTy [] ->
        if List.exists ~f:(function TuplePat [] -> true | _ -> false) pats
        then return None
        else return (Some (Tuple []))
    | TupleTy tup ->
        let pats =
          List.map
            ~f:(function TuplePat v -> v | _ -> raise Would_not_unify)
            pats
          |> transpose
        in

        let%map counter_examples =
          List.fold2_exn ~init:(return [])
            ~f:(fun state ty pats ->
              let%bind l = state in
              let%map v = pattern_total ty pats in
              v :: l)
            tup pats
        in
        if List.exists ~f:Option.is_some counter_examples then
          Some
            (Tuple
               (List.rev_map counter_examples ~f:(function
                 | None -> Expr.Id 0
                 | Some v -> v)))
        else None
    | Var _ | Arrow _ -> fail (* value can only be matched by binding pat *)

let pattern_total ty pats flat =
  let open Option.Let_syntax in
  let%map _, v = pattern_total ty pats flat in
  v
