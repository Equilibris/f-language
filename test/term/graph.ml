open! Core
open Irs.Flat
open Flang.Term.Graph

let viz ~to_string =
  Hashtbl.fold ~init:"" ~f:(fun ~key ~data acc ->
      let source = key in
      Hashtbl.fold ~init:acc
        ~f:(fun ~key ~data acc ->
          sprintf "%s\n%s -- %i --> %s" acc (to_string source) data
            (to_string key))
        data)

let g =
  let nat =
    {|
      type nat = Zero () | Succ nat;

      funny = \x \y \z match x with
        | Succ Succ Succ n -> funny (Succ y) (Succ z) n
        | _ -> Zero ();

      ack = \ax \ay match ax with
        | Zero () -> Succ ay
        | Succ x -> ack x 
            (match ay with
            | Zero _  -> Succ (Zero ())
            | Succ ay -> ack ax ay);
    |}
  in

  let nat = convert nat in
  let _, v = enter_state () nat |> Option.value_exn in
  let v = viz ~to_string:(Map.find_exn nat.ens.i2s) v in
  (* let v = viz ~to_string:Int.to_string v in *)
  print_endline v
