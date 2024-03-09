open! Core
open Flang.Types
open Flang.Irs.Ast
open Core.Option.Let_syntax
open Core.Poly

let unify a b =
  let%bind ty_map, t = unify Int.equal a b (Map.empty (module Int)) in
  let%map t = replace_var_deep t ty_map in
  t

let () =
  assert (
    unify (TupleTy [ Id 0; Id 1; Id 2 ]) (TupleTy [ Id 0; Id 1; Id 2 ])
    = Some (TupleTy [ Id 0; Id 1; Id 2 ]))

let () =
  assert (
    unify
      (TupleTy [ Var 0; Var 0; Var 1 ])
      (TupleTy [ Var 2; TupleTy []; Var 3 ])
    = Some (TupleTy [ TupleTy []; TupleTy []; Var 3 ]))

let _ = assert (unify (TupleTy []) (Var 0) = Some (TupleTy []))
let _ = assert (unify (Var 0) (TupleTy []) = Some (TupleTy []))

let _ =
  assert (
    unify (Arrow { i = Var 0; o = Var 0 }) (Arrow { i = Id 0; o = Id 0 })
    = Some (Arrow { i = Id 0; o = Id 0 }))
