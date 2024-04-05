open Objects

(**
    The elements of this semi-ring are partially gathered from the arctic
    semi-ring and partially come from Feotus. Heres an explanation of
    the elements below:
    - Accurate  |-> This stores accurate size information this comes
                    from the arctic
    - NegInf    |-> Zero, also comes from the arctic
    - Unrelated |-> Corresponds to half of ? in Feotus (as in the paper
                    its also gt)
    - Inf       |-> Corresponds to the greater than relation in Feotus

    Elements we remove from Feotus

    - =         |-> Accurate 0
    - <         |-> Accurate -1 ( worst case )

    New possible observation just dropped: isn't unrelated the same as
    Acc 0... no it is not.

    Observe how the mult tables of * and + line up with feotuses

    +------------------+
    |  + | -∞ |  0 |  ∞|
    |------------------|
    | -∞ | -∞ | -∞ | -∞|
    |  0 | -∞ |  0 |  0|
    |  ∞ | -∞ |  0 |  ∞|
    +------------------+

    +------------------+
    |  * | -∞ |  0 |  ∞|
    |------------------|
    | -∞ | -∞ | -∞ | ∞*|
    |  0 | -∞ |  0 |  ∞|
    |  ∞ | ∞* |  ∞ |  ∞|
    +------------------+

    * = we define this to be the case
 *)

type qt = Acc of int [@equal Int.equal] | NegInf | Inf [@@deriving eq]

let show_t = function Acc v -> Int.to_string v | NegInf -> "-∞" | Inf -> "∞"

module TheRig : RigFix with type t = qt = struct
  type t = qt

  let one = Acc 0
  let zero = Inf

  let ( * ) a b =
    match (a, b) with
    | Acc a, Acc b -> Acc (a + b)
    (* This needs to be highlighted *)
    | Inf, NegInf | NegInf, Inf -> Inf
    | Inf, _ | _, Inf -> Inf
    | NegInf, _ | _, NegInf -> NegInf

  let ( + ) a b =
    match (a, b) with
    | Acc a, Acc b -> Acc (Int.min a b)
    | NegInf, _ | _, NegInf -> NegInf
    | Inf, x | x, Inf -> x

  let ( = ) = equal_qt

  let fix = function
    | Acc a -> (
        match Int.compare a 0 with 0 -> Acc 0 | 1 -> Inf | _ -> NegInf)
    | a -> a
end

type t = qt
