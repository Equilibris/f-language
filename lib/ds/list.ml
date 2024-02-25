open Core
include Core.List

let map_opt ~f ls =
  List.fold_until ~init:[] ~finish:Option.return
    ~f:(fun acc x ->
      match f x with
      | None -> Continue_or_stop.Stop None
      | Some v -> Continue_or_stop.Continue (v :: acc))
    ls
