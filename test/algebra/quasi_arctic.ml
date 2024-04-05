open! Core
open Flang.Algebra
open Option.Let_syntax
module M = Mat.RigFixMat (TheRig)

let rec big_pow v n =
  if n > 1 then
    let%bind v = M.(v * v) in
    big_pow v (n - 1)
  else return v

let () =
  (let open! M in
   let%bind res = M.of_array (2, 2) [| Acc 10; Acc 10; Acc 2; Acc 10 |] in
   (* let%bind v = M.fix (M.one 2) in *)
   let%bind v = big_pow res 10 in

   M.to_string ~to_string:show_t res |> print_endline;
   M.to_string ~to_string:show_t v |> print_endline;
   Some ())
  |> Option.value_exn
