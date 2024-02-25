open Core

type 'a constructor_pat = 'a * 'a pat
and 'a tuple_pat = 'a pat list

and 'a pat =
  | BindingPat of 'a
  | TuplePat of 'a tuple_pat
  | ConstructorPat of 'a constructor_pat
[@@deriving show, eq]

let rec pat_to_src show_a = function
  | BindingPat v -> show_a v
  | TuplePat v ->
      "("
      ^ (List.map ~f:(fun v -> pat_to_src show_a v ^ ", ") v |> String.concat)
      ^ ")"
  | ConstructorPat (constructor, children) ->
      show_a constructor ^ "(" ^ pat_to_src show_a children ^ ")"
