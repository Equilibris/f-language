open! Core
open Irs.Flat
open Flang.Term.Seek_traces
open Flang.Irs.Flat
open Core.Poly

let canon_alist = List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
let nat = " type nat     = Zero () | Succ nat;"
let list = "type list 'a =  Nil () | Cons ('a, list 'a);"
let m_empty = Map.empty (module Int)

let get_traces_of flat id =
  let def_map = fn_def_map flat in
  let expr = Map.find_exn def_map id in
  get_traces expr m_empty

let _ =
  let inc_dec_dec =
    String.append nat
      {|
        v = \x match Succ x with
            | Succ Succ x -> v x
            | _           -> Zero ();
      |}
  in

  let flat = convert inc_dec_dec in
  let traces, _ = get_traces_of flat 2 |> Option.value_exn in

  (* Map.find_exn traces 4 |> show_trace Format.pp_print_int |> print_endline; *)
  assert (Map.find_exn traces 4 = [ Dec 1; Dec 1; Inc 1; TId 3 ])
