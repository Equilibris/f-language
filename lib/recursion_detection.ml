open Core
open Ast

let code_array_transform i code =
  let code_map = Array.init i ~f:(fun _ -> None) in
  let deps = Array.init i ~f:(fun _ -> []) in
  let rec marcher target = function
    | Lit _ -> ()
    | Id v -> deps.(target) <- v :: deps.(target)
    | Tuple xs -> List.iter ~f:(marcher target) xs
    | Bind { name; value; within } ->
        deps.(target) <- name :: deps.(target);
        code_map.(name) <- Some value;
        marcher name within
    | Condition { predicate; t_branch; f_branch } ->
        marcher target predicate;
        marcher target t_branch;
        marcher target f_branch
    | Call { callee; arg } ->
        marcher target callee;
        marcher target arg
    | Lambda { binding; content } ->
        code_map.(binding) <- Some (Id binding);
        marcher target content
    | Constructor (_, x) ->
        (* deps.(target) <- v :: deps.(target); *)
        (* The reason we dont include this line on purpose is they are never
                      recursive and thereby they will be removed in the next stage anyways. *)
        marcher target x
  in
  let top_level_names =
    List.fold ~init:[]
      ~f:(fun curr -> function
        | Decl { name; expr } ->
            marcher name expr;
            code_map.(name) <- Some expr;
            name :: curr
        | DeclTy _ -> curr (* Skip for now as we dont trust sigs *)
        | TyDef { name = _; vars = _; constructors } ->
            List.iter
              ~f:(fun { constructor; ty = _ } ->
                code_map.(constructor) <- Some (Id constructor))
              constructors;
            curr)
      code
  in

  ( top_level_names,
    Array.mapi
      ~f:(fun i v -> Option.value_exn ~message:(Int.to_string i) v)
      code_map,
    deps )

module IndexSet : sig
  type ('a, 'cmp) t

  val empty : ('a, 'b) Base.Comparator.Module.t -> ('a, 'b) t
  val enqueue : ('a, 'b) t -> 'a -> ('a, 'b) t
  val pop : ('a, 'b) t -> 'a option * ('a, 'b) t
  val mem : ('a, 'b) t -> 'a -> bool
end = struct
  (* Leaves an implicit assumtion that each enqueued entry is unique as if
     you enqueue the same value twice then pop it it will leave the
     invariant that a value is in both the set and the list unsatasfied *)
  type ('a, 'cmp) t = 'a list * ('a, 'cmp) Base.Set.t

  let empty m = ([], Base.Set.empty m)
  let enqueue (xs, set) value = (value :: xs, Base.Set.add set value)
  let mem (_, set) value = Base.Set.mem set value

  let pop ((xs, set) as v) =
    match xs with
    | [] -> (None, v)
    | x :: xs -> (Some x, (xs, Base.Set.remove set x))
end

(* Would be trivial to caclulate all recursive
   paths rather than just a t/f value *)
let recalculate_recursion deps =
  let i = Array.length deps in
  let visited () = Array.init ~f:(fun _ -> false) i in
  let recursive = visited () in
  let rec dfs_iter chain visited i =
    if visited.(i) then ()
    else if IndexSet.mem chain i then recursive.(i) <- true
    else
      let chain = IndexSet.enqueue chain i in
      List.iter ~f:(dfs_iter chain visited) deps.(i);
      visited.(i) <- true
  in
  let rec iter curr =
    if curr = i then ()
    else (
      dfs_iter (IndexSet.empty (module Int)) (visited ()) curr;
      iter (curr + 1))
  in
  iter 0;
  recursive

module Tests = struct
  open De_bruijn_transform.Tests
  open Core.Poly

  let%test _ =
    let ens, _, v = parse_and_convert rec_test in
    let i = De_bruijn_transform.Namespace.i ens in
    let tln, code, deps = code_array_transform i v in
    let recs = recalculate_recursion deps in
    recs = [| true; false; true; false |]
    && deps = [| [ 1; 2 ]; []; [ 3; 0 ]; [] |]
    && tln = [ 2; 0 ]
    && code
       = [|
           Lambda { binding = 1; content = Call { callee = Id 2; arg = Id 1 } };
           Id 1;
           Lambda { binding = 3; content = Call { callee = Id 0; arg = Id 3 } };
           Id 3;
         |]

  (* let%test_unit _ = *)
  (*   let ens, _, v = parse_and_convert rec_test in *)
  (*   let i = De_bruijn_transform.Namespace.i ens in *)
  (*   let _tln, code, _deps = code_array_transform i v in *)
  (*   List.of_array code *)
  (*   |> List.map ~f:(expr_to_src Int.to_string) *)
  (*   |> String.concat |> print_endline *)
end
