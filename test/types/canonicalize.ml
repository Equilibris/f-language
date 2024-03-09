open Core
open Flang.Types
open Flang.Irs.Ast.Type

let canonicalize x = canonicalize x (Type_map.empty (module Int))

let ( =* ) a b =
  let _, v = canonicalize a in
  assert (equal_ty Int.equal b v)

let () = Id 0 =* Id 0
let () = Var 1 =* Var 0
let () = Arrow { i = Var 1; o = Var 1 } =* Arrow { i = Var 0; o = Var 0 }
let () = Arrow { i = Var 2; o = Var 1 } =* Arrow { i = Var 0; o = Var 1 }

let () =
  Applicative { ty = Id 0; arg = Var 1 }
  =* Applicative { ty = Id 0; arg = Var 0 }

let () = TupleTy [] =* TupleTy []
let () = TupleTy [ Id 0 ] =* TupleTy [ Id 0 ]
let () = TupleTy [ Var 1 ] =* TupleTy [ Var 0 ]
let () = TupleTy [ Id 0; Id 1 ] =* TupleTy [ Id 0; Id 1 ]
let () = TupleTy [ Id 0; Id 1; Id 10 ] =* TupleTy [ Id 0; Id 1; Id 10 ]
