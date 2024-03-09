open! Core
open Flang.Types
open Flang.Irs.Ast

let ty_map = Type_map.empty (module Int)

let () =
  let _, nt = canonicalize (Arrow { i = Var 10; o = Var 9 }) ty_map in
  assert (equal_ty Int.equal nt (Arrow { i = Var 0; o = Var 1 }))

let () =
  let _, nt = canonicalize (Arrow { i = Var 100; o = Var 100 }) ty_map in
  assert (equal_ty Int.equal nt (Arrow { i = Var 0; o = Var 0 }))

let () =
  let ty_map, nt1 = canonicalize (Arrow { i = Var 100; o = Var 100 }) ty_map in
  let _, nt2 = canonicalize (Arrow { i = Var 100; o = Var 101 }) ty_map in
  assert (
    equal_ty Int.equal nt1 (Arrow { i = Var 0; o = Var 0 })
    && equal_ty Int.equal nt2 (Arrow { i = Var 1; o = Var 2 }))
