open! Core
open Irs.Ast

let extract_calls expr =
  let curr_calls = Hashtbl.create (module Int) in
  let rec explore = function
    | Call c ->
        let fn, call = handle_call c in

        Hashtbl.update curr_calls fn ~f:(fun v ->
            call :: Option.value v ~default:[])
    | Bind { name = _; value; within } ->
        explore value;
        explore within
    | Match { scrutinee; arms } ->
        explore scrutinee;
        List.iter ~f:(fun (_, b) -> explore b) arms
    | Tuple t -> List.iter t ~f:explore
    | Lambda { binding = _; content = v } | Constructor (_, v) -> explore v
    | Id _ -> ()
  and handle_call { callee; arg } =
    match callee with
    | Id id -> (id, [ arg ])
    | Call v ->
        let id, curr = handle_call v in
        explore arg;
        (id, arg :: curr)
    (* This can be made easier using the list-monad i thonk *)
    | Bind _ | Match _ | Lambda _ -> failwith "todo"
    | Tuple _ | Constructor _ -> failwith "Would not unify"
  in

  explore expr;
  curr_calls
