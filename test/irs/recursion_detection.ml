open De_burijn_transform
open Core.Poly
open Flang.Irs
open Flang.Ds
open Recursion_detection

let _ =
  let ens, _, v = parse_and_convert rec_test in
  let i = Namespace.i ens in
  let tln, code, deps = code_array_transform i v in
  let recs = recalculate_recursion deps in
  assert (
    recs = [| true; false; true; false |]
    && deps = [| [ 1; 2 ]; []; [ 3; 0 ]; [] |]
    && tln = [ 2; 0 ]
    && code
       = [|
           Lambda { binding = 1; content = Call { callee = Id 2; arg = Id 1 } };
           Id 1;
           Lambda { binding = 3; content = Call { callee = Id 0; arg = Id 3 } };
           Id 3;
         |])
