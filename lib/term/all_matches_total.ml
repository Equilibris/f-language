open Core
open Types
open Pat_total
open Irs.Ast
open Ds.State_opt
open Ds.State_opt.Let_syntax
open Types.State.Setters (Option)

(**
    Finds out if an entire function is total. This requires type
    information which is why its useful to also provide a type map and
    not just a flat ir.
 *)
let rec all_total leading = function
  | Match { scrutinee; arms } ->
      let%bind ty = infer_expr eset scrutinee in
      let pats = List.map arms ~f:(fun (p, _) -> p) in
      let%bind v = pattern_total ty pats |> effectless |> set_flat_ir in
      let leading = v :: leading in
      let%map inter_leading =
        List.fold ~init:(return [])
          ~f:(fun state (_, e) ->
            let%bind leading = state in
            all_total leading e)
          arms
      in
      List.rev inter_leading @ leading
  | Bind { name = _; value = a; within = b } | Call { callee = a; arg = b } ->
      let%bind leading = all_total leading a in
      all_total leading b
  | Lambda { binding = _; content } -> all_total leading content
  | Tuple t ->
      let%map inter_leading =
        List.fold ~init:(return [])
          ~f:(fun state next ->
            let%bind leading = state in
            all_total leading next)
          t
      in

      List.rev inter_leading @ leading
  | Constructor (_, v) -> all_total leading v
  | Id _ -> return leading
