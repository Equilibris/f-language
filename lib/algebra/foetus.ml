open Objects

type foetus = Lt | Uk | Eq [@@deriving eq, show]

module Foetus : RigFix with type t = foetus = struct
  type t = foetus

  let one = Eq
  let zero = Uk

  let ( + ) a b =
    match (a, b) with Lt, _ | _, Lt -> Lt | Eq, _ | _, Eq -> Eq | Uk, Uk -> Uk

  let ( * ) a b =
    match (a, b) with Uk, _ | _, Uk -> Uk | Lt, _ | _, Lt -> Lt | Eq, Eq -> Eq

  let ( = ) = equal_foetus
  let fix = Fun.id
end
