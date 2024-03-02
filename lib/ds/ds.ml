module Index_set = Index_set
module Namespace = Namespace
module List = List

(* So if we reason about value, it is a function from a state -> v * state
   if re repeat this we can treat it as simply a function on v allowing us to
   let the rest be of v -> v' so we in total get:
*)
module TrivialState : sig
  type ('a, 'state) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'state) t
    val ( >>= ) : ('a, 'state) t -> ('a -> ('b, 'state) t) -> ('b, 'state) t
    val ( >>| ) : ('a, 'state) t -> ('a -> 'b) -> ('b, 'state) t
  end
end = struct
  (* https://www.cryptologie.net/article/581/state-monads-in-ocaml/ *)
  type ('a, 'state) t = 'state -> 'a * 'state

  module Let_syntax = struct
    let return a state = (a, state)

    let ( >>= ) t f state =
      let a, transient_state = t state in
      let b, final_state = f a transient_state in
      (b, final_state)

    let ( >>| ) t f state =
      let a, final_state = t state in
      let b = f a in
      (b, final_state)
  end
end

module State_t (State : Base.T) (A : Base.Monad.S) = struct
  module Inner = struct
    type 'a t = State.t -> ('a * State.t) A.t

    let return a state = A.return (a, state)

    let bind t ~f state =
      let open A.Let_syntax in
      let%bind a, transient_state = t state in
      let%map b, final_state = f a transient_state in
      (b, final_state)

    let map = `Define_using_bind
    let inspect : State.t t = fun state -> A.return (state, state)
  end

  include Base.Monad.Make (Inner)
  include Inner
end
