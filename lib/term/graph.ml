open! Core
open Irs
open Ds
open Arg_extract
open Call_extraction

module State = struct
  type ('a0, 'b0, 'c0) t = {
    flat_ir : ('a0, 'b0, 'c0) Flat.flat_ir;
    arg_map : (int, int list, 'c0) Map.t;
    calls : (int, (int, int Ast.expr list list) Hashtbl.t, 'c0) Map.t;
  }

  module Setters (M : Base.Monad.S) = struct
    module M = State_t (M)
    open M

    let set_flat_ir x =
      translate
        (fun { flat_ir; arg_map = _; calls = _ } -> flat_ir)
        (fun flat_ir s -> { s with flat_ir })
        x

    let set_arg_map x =
      translate
        (fun { flat_ir = _; arg_map; calls = _ } -> arg_map)
        (fun arg_map s -> { s with arg_map })
        x

    let set_calls x =
      translate
        (fun { flat_ir = _; arg_map = _; calls } -> calls)
        (fun calls s -> { s with calls })
        x
  end
end

open State

let enter_state flat_ir =
  let fn_def_map =
    Flat.fn_def_map flat_ir |> Map.map ~f:(fun { name = _; expr } -> expr)
  in
  let arg_map = fn_def_map |> Map.map ~f:get_args in
  let calls = fn_def_map |> Map.map ~f:extract_calls in
  { flat_ir; arg_map; calls }
