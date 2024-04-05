open! Core
open Option.Let_syntax
open Flang.Algebra
module M = Mat.RigMat (Int)

let ring_correct_tester (type t) (module R : Objects.Rig with type t = t)
    special_objects to_string =
  let module M = Mat.RigMat (R) in
  let%map special_matricies =
    (let open List.Let_syntax in
     let%bind a = special_objects in
     let%bind b = special_objects in
     let%bind c = special_objects in
     let%map d = special_objects in
     M.of_array (2, 2) [| a; b; c; d |])
    |> Option.all
  in
  let id = M.one 2 in
  let failing_matricies =
    List.filter
      ~f:(fun mat ->
        M.(
          let%map v = id * mat in
          v = mat |> not)
        |> Option.value_exn)
      special_matricies
  in
  if List.length failing_matricies = 0 then ()
  else (
    List.iter
      ~f:(fun v ->
        printf "The matrix:\n";
        M.to_string ~to_string v |> printf "%s";
        printf "when multiplied with\n";
        M.to_string ~to_string id |> printf "%s";
        printf "becomes:\n";
        M.(v * id)
        |> Option.value_exn |> M.to_string ~to_string |> printf "%s\n")
      failing_matricies;
    printf "%!";
    assert false)

let () =
  ring_correct_tester (module Foetus) [ Lt; Uk; Eq ] show_foetus
  |> Option.value_exn

let () =
  ring_correct_tester (module Int) [ -1; 0; 1 ] Int.to_string
  |> Option.value_exn

let () =
  ring_correct_tester
    (module TheRig)
    [ Inf; NegInf; Acc 0; Acc (-1); Acc 1 ]
    show_t
  |> Option.value_exn
