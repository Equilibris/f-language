(* Leaves an implicit assumtion that each enqueued entry is unique as if
   you enqueue the same value twice then pop it it will leave the
   invariant that a value is in both the set and the list unsatasfied *)
type ('a, 'cmp) t = 'a list * ('a, 'cmp) Base.Set.t

let empty m = ([], Base.Set.empty m)
let enqueue (xs, set) value = (value :: xs, Base.Set.add set value)
let mem (_, set) value = Base.Set.mem set value

let pop ((xs, set) as v) =
  match xs with
  | [] -> (None, v)
  | x :: xs -> (Some x, (xs, Base.Set.remove set x))
