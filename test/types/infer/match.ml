open! Core
open Flang.Types
open Flang.Irs.Ast
open Flang.Ds
open Irs.Flat
open Core.Option.Let_syntax

let ty_map = Type_map.empty (module Int)

let _ =
  let flat =
    convert
      {|
          type unit = Unit ();

          unit = \x Unit ();
        |}
  in
  let%map _, _, id_ty = gather_top_level flat 1 in
  let _, id_ty = canonicalize (Type_map.empty (module Int)) id_ty in
  assert (equal_ty Int.equal id_ty (Arrow { i = Var 0; o = Id 0 }))

let _ =
  let flat =
    convert
      {|
          type gen 'a = Gen 'a;

          gen = \x Gen x;
        |}
  in
  let%map _, _, id_ty = gather_top_level flat 1 in
  let _, id_ty = canonicalize (Type_map.empty (module Int)) id_ty in
  assert (
    equal_ty Int.equal id_ty
      (Arrow { i = Var 0; o = Applicative { ty = Id 0; arg = Var 0 } }))

let _ =
  let flat =
    convert
      {|
        type gen 'a = Gen 'a;
        de_gen = \x match x with | Gen v -> v;
      |}
  in
  let%map _, _, de_gen_ty = gather_top_level flat 1 in

  let _, de_gen_ty = canonicalize (Type_map.empty (module Int)) de_gen_ty in
  assert (
    equal_ty Int.equal de_gen_ty
      (Arrow { i = Applicative { ty = Id 0; arg = Var 0 }; o = Var 0 }))

let _ =
  let flat =
    convert
      {|
            type nat = Succ nat | Zero ();
            type list 'a = Nil () | Cons ('a, list 'a);

            len = \x match x with | Nil _ -> Zero () | Cons (_, x) -> Succ (len x);
          |}
  in
  let%map _, _, len_ty = gather_top_level flat 4 in
  let _, len_ty = canonicalize (Type_map.empty (module Int)) len_ty in

  assert (
    equal_ty Int.equal len_ty
      (Arrow { i = Applicative { ty = Id 1; arg = Var 0 }; o = Id 0 }))

let _ =
  let _, _, flat =
    convert_with_map
      {|
           restrict_id = \x
               match x with
               | (a, b) -> x;
         |}
  in
  let%map _, _ty_map, restrict_id = gather_top_level flat 0 in
  let _, restrict_id = canonicalize (Type_map.empty (module Int)) restrict_id in
  assert (
    equal_ty Int.equal restrict_id
      (Arrow { i = TupleTy [ Var 0; Var 1 ]; o = TupleTy [ Var 0; Var 1 ] }))

let _ =
  let _, _, flat =
    convert_with_map
      {|
        type list 'a = Nil () | Cons ('a, list 'a);

        loop = \x loop x;

        map = \f \x
            match x with
            | Nil () -> Nil ()
            | Cons (v, x) -> Cons (f v, map f x);
      |}
  in
  let%map _, _, map_ty = gather_top_level flat 4 in
  let _, map_ty = canonicalize (Type_map.empty (module Int)) map_ty in
  assert (
    equal_ty Int.equal map_ty
      (Arrow
         {
           i = Arrow { i = Var 0; o = Var 1 };
           o =
             Arrow
               {
                 i = Applicative { ty = Id 0; arg = Var 0 };
                 o = Applicative { ty = Id 0; arg = Var 1 };
               };
         }))

let _ =
  let flat = convert {|
    f = \x g x;
    g = \x f x;
  |} in
  Map.keys flat.fn_def_map
  |> List.map ~f:(fun v ->
         let%map _, _, ty = gather_top_level flat v in
         let _, ty = canonicalize (Type_map.empty (module Int)) ty in

         assert (equal_ty Int.equal ty (Arrow { i = Var 0; o = Var 1 })))
