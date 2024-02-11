open Core

type ('a, 'cmp) t = { ls : 'a list; map : ('a, int, 'cmp) Map.t }

let empty m = { ls = []; map = Map.empty m }

let enqueue { ls; map } value =
  {
    ls = value :: ls;
    map = Map.update ~f:(Option.value_map ~f:Int.succ ~default:1) map value;
  }

(* Can be done in O(1) time with an addition of O(1) mem *)
let last { ls; map = _ } = List.last ls
let ls { ls; map = _ } = ls
let mem { ls = _; map } value = Map.mem map value

let pop ({ ls = xs; map = set } as v) =
  match xs with
  | [] -> (None, v)
  | x :: xs ->
      ( Some x,
        {
          ls = xs;
          map =
            Map.change
              ~f:(fun v ->
                v
                |> Option.map ~f:(fun x -> x - 1)
                |> Option.filter ~f:Int.is_positive)
              set x;
        } )
