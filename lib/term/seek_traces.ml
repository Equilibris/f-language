open! Core
open Irs.Ast
open Ds.State_opt
open Ds.State_opt.Let_syntax

(*
       A trace should track how a given variable relates to other variables,
       this is done by walking through the program tracking each time we do
       a variable destructure. We might have to say that most variables in a
       bind are unrelated but only the recursive ones are. There might be some
       clique-finding involved and in general all we want to do is walk through
       a recursive type and mark when we decrease, then find this as well in
       the function.

       These feel a lot like 'structural height types', this is maybe an
       idea worth looking into as it means we can paint function types with
       structural information and then let inference solve for termination
       and so on... this feels like doing inference on kinds... finding
       the structural 'kind' of a given recursive type. This is a very fun
       project to try :)

   > type nat = Zero () | Succ nat

       Would get the kind:

   > type nat = Zero ? | Succ -1

       How would this work in mutually rec types ? Do types always have
       just one kind ?

       This was a good idea but I think it might be better to simply
       track different constructor additions and subtractions. To do this
       all we need is a list which is honestly far easier than the kinds
       option. This also feels more like actual traces. 

       These traces seem to form forests for the given input, how do we
       handle this? For now well just stick to lists as they have nice
       memory properties :)
   *)

type 'a delta =
  | TId of 'a
  | Inc of 'a
  | Dec of 'a
  | Tuple of 'a trace list
  | Union of 'a trace list

and 'a trace = 'a delta list [@@deriving show]

type ('a, 'cmp) traces = ('a, 'a trace, 'cmp) Map.t

let rec pat_trace data = function
  | TuplePat t ->
      List.fold ~init:(return ())
        ~f:(fun state v ->
          let%bind () = state in
          pat_trace data v)
        t
  | BindingPat key -> Map.set ~key ~data |> i_ret |> effect
  | ConstructorPat (constructor, deep) ->
      pat_trace (Dec constructor :: data) deep

let rec get_traces = function
  | Constructor (addition, expr) ->
      let%map trace = get_traces expr in
      Inc addition :: trace
  | Match { scrutinee; arms } ->
      let%bind trace = get_traces scrutinee in
      let%bind () =
        List.fold ~init:(return ())
          ~f:(fun state (pat, _) ->
            let%bind () = state in
            pat_trace trace pat)
          arms
      in
      let%map v =
        List.fold ~init:(return [])
          ~f:(fun state (_, expr) ->
            let%bind last = state in
            get_traces expr >>| Fn.flip List.cons last)
          arms
      in
      [ Union v ]
  | Bind { name = key; value; within } ->
      let%bind data = get_traces value in
      let%bind () = Map.set ~key ~data |> i_ret |> effect in
      get_traces within
  | Lambda { binding = key; content } ->
      let%bind () = Map.set ~key ~data:[ TId key ] |> i_ret |> effect in
      get_traces content
  | Tuple l ->
      let%map x =
        List.fold l ~init:(return []) ~f:(fun state v ->
            let%bind last = state in
            let%map x = get_traces v in

            x :: last)
      in
      [ Tuple (List.rev x) ]
  | Call { callee = _; arg } ->
      (* let%bind _ = get_traces callee in *)
      let%map _ = get_traces arg in
      []
  (* TODO: This require deeper trace types... This is an inference
              job wtf *)
  | Id v -> Fn.flip Map.find v |> effectless

exception Malformed_input

let rec flatten_trace ~pred v =
  let open List.Let_syntax in
  match v with
  | [] -> []
  | TId v :: [] -> return (v, 0)
  | Inc constructor :: rest ->
      let%map a, b = flatten_trace ~pred rest in
      (a, b + if pred constructor then 1 else 0)
  | Dec constructor :: rest ->
      let%map a, b = flatten_trace ~pred rest in
      (a, b - if pred constructor then 1 else 0)
  | Union traces :: [] | Tuple traces :: [] ->
      let%bind trace = traces in
      flatten_trace ~pred trace
  | _ -> raise Malformed_input
