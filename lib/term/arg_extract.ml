open! Core
open Irs.Ast
(*
    This is acc a suprisingly hard problem but its far simpler to do it
    without any type information and just say that partial calls are
    illegal for termination checking. To actually solve this is far
    harder i think

    This is kinda an inference job
 *)

(* module State = struct
     type ('a, 'b, 'c, 'd, 'e, 'f) t = {
       flat_ir : ('a, 'b, 'c) Flat.flat_ir;
       mp : ('d, 'e, 'f) Map.t;
     }

     module Setters (M : Base.Monad.S) = struct
       module M = State_t (M)
       open M

       let set_mp x =
         translate (fun { flat_ir = _; mp } -> mp) (fun mp s -> { s with mp }) x

       let set_flat_ir x =
         translate
           (fun { flat_ir; mp = _ } -> flat_ir)
           (fun flat_ir s -> { s with flat_ir })
           x
     end
   end *)

let rec get_args = function
  | Lambda { binding; content } -> binding :: get_args content
  | _ -> []
