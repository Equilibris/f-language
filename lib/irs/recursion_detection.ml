open Core
open Ast
open Ds

let code_array_transform i code =
  let code_map = Array.init i ~f:(fun _ -> None) in
  let deps = Array.init i ~f:(fun _ -> []) in
  let rec marcher target = function
    | Tuple xs -> List.iter ~f:(marcher target) xs
    | Id v -> deps.(target) <- v :: deps.(target)
    | Bind { name; value; within } ->
        deps.(target) <- name :: deps.(target);
        code_map.(name) <- Some value;
        marcher name within
    | Match { scrutinee; arms } ->
        marcher target scrutinee;
        List.iter ~f:(fun (_, v) -> marcher target v) arms
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

(* Would be trivial to caclulate all recursive
   paths rather than just a t/f value *)
let recalculate_recursion deps =
  let i = Array.length deps in
  let visited () = Array.init ~f:(fun _ -> false) i in
  let recursive = visited () in
  let rec dfs_iter chain visited i =
    if visited.(i) then ()
    else if Index_set.mem chain i then recursive.(i) <- true
    else
      let chain = Index_set.enqueue chain i in
      List.iter ~f:(dfs_iter chain visited) deps.(i);
      visited.(i) <- true
  in
  let rec iter curr =
    if curr = i then ()
    else (
      dfs_iter (Index_set.empty (module Int)) (visited ()) curr;
      iter (curr + 1))
  in
  iter 0;
  recursive
