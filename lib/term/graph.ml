open! Core
open Irs.Flat.Setters (Option)
open Ds.State_opt
open Ds.State_opt.Let_syntax
open Arg_extract
open Seek_traces
open Call_extraction

let enter_state () =
  let%bind trace_map, _ =
    Fn.flip
      (Map.fold ~init:(return ()) ~f:(fun ~key ~data v ->
           (* TODO: remove *)
           let _ = key in
           let%bind () = v in
           let%map _ = get_traces data in
           ()))
      (Map.empty (module Int))
    |> effectless |> set_fn_def_map
  in
  let%bind arg_map =
    Map.map ~f:get_args |> i_ret |> effectless |> set_fn_def_map
  in
  let%map calls =
    Map.map ~f:extract_calls |> i_ret |> effectless |> set_fn_def_map
  in

  let base = Hashtbl.create (module Int) in

  let each_call_set ~key ~data =
    (* Should this be rev *)
    let arg_names = Map.find_exn arg_map key |> List.rev in
    List.iter data
      ~f:
        (List.iter2_exn arg_names ~f:(fun target source ->
             let _, source = get_traces source trace_map |> Option.value_exn in
             let traces = flatten_trace ~pred:(Fn.const true) source in
             List.iter traces ~f:(fun (source, value) ->
                 Hashtbl.update base source ~f:(fun x ->
                     let x =
                       Option.value_or_thunk x ~default:(fun () ->
                           Hashtbl.create (module Int))
                     in
                     Hashtbl.update x target
                       ~f:
                         (Fn.compose (Int.max value)
                            (Option.value ~default:value));
                     x))))
  in

  Map.iter calls ~f:(Hashtbl.iteri ~f:each_call_set);
  base
