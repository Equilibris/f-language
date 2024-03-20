open! Core
open Irs.Flat
open Flang.Term.Pat_total
open Flang.Irs.Ast
open Core.Poly

let nat = " type nat     = Zero () | Succ nat;"
let list = "type list 'a =  Nil () | Cons ('a, list 'a);"
let () = assert (transpose [ [ 0; 1 ]; [ 0; 0 ] ] = [ [ 0; 0 ]; [ 0; 1 ] ])
let nat_flat = convert nat
let list_flat = convert (String.append nat list)
let empty_flat = convert ""

(* var examples *)

let () = assert (pattern_total (Var 0) [] empty_flat = Some (Some (Id 0)))
let () = assert (pattern_total (Var 0) [ BindingPat 0 ] empty_flat = Some None)

(* trivial tup examples *)

let () =
  assert (
    pattern_total (TupleTy [ TupleTy [] ]) [] empty_flat
    = Some (Some (Expr.Id 0)))

let () =
  assert (
    pattern_total (TupleTy [ TupleTy [] ])
      [ TuplePat [ TuplePat [] ] ]
      empty_flat
    = Some None)

let () =
  assert (
    pattern_total
      (TupleTy [ TupleTy []; TupleTy [] ])
      [ TuplePat [ TuplePat []; TuplePat [] ] ]
      empty_flat
    = Some None)

(* constructor *)

let () =
  assert (
    pattern_total (Type.Id 0) [ ConstructorPat (0, TuplePat []) ] nat_flat
    = Some (Some (Constructor (1, Id 0))))

let () =
  assert (
    pattern_total (Type.Id 0)
      (* solve for x when x != and x < 2 *)
      [
        ConstructorPat (1, ConstructorPat (1, BindingPat 2));
        ConstructorPat (0, TuplePat []);
      ]
      nat_flat
    = Some (Some ((* x = 1 *)
                    Constructor (1, Constructor (0, Id 0)))))

(* let () = print *)

(* let () = *)
(*   pattern_total *)
(*     Type.(Applicative { ty = Id 1; arg = Id 0 }) *)
(*     [ ConstructorPat (2, BindingPat 0) ] *)
(*     list_flat *)
(*   |> Option.value_exn ~message:"0" *)
(*   |> Option.value_exn ~message:"1" *)
(*   |> show_expr Format.pp_print_int *)
(*   |> print_endline *)
