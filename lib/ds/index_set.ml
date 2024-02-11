open Core

type ('a, 'cmp) t = T of 'a list * ('a, int, 'cmp) Map.t

let empty m = T ([], Map.empty m)

let enqueue (T (xs, m)) value =
  T
    ( value :: xs,
      Map.update ~f:(Option.value_map ~f:Int.succ ~default:1) m value )

let last (T (xs, _)) = List.last xs
let ls (T (xs, _)) = xs
let mem (T (_, set)) value = Map.mem set value

let pop (T (xs, set) as v) =
  match xs with
  | [] -> (None, v)
  | x :: xs ->
      ( Some x,
        T
          ( xs,
            Map.change
              ~f:(fun v ->
                v
                |> Option.map ~f:(fun x -> x - 1)
                |> Option.filter ~f:Int.is_positive)
              set x ) )
