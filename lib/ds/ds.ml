module Index_set = Index_set
module Namespace = Namespace
module List = List

module State_t (Meta : Base.Monad.S) = struct
  module Inner = struct
    type ('a, 'state) t = 'state -> ('a * 'state) Meta.t

    let return a state = Meta.return (a, state)

    let bind t ~f state =
      let open Meta.Let_syntax in
      let%bind a, transient_state = t state in
      let%map b, final_state = f a transient_state in
      (b, final_state)

    let map = `Define_using_bind
    let inspect state = Meta.return (state, state)
    let assign s _ = Meta.return ((), s)

    let translate :
        ('state -> 'n_state) ->
        ('n_state -> 'state -> 'state) ->
        ('b, 'n_state) t ->
        ('b, 'state) t =
     fun t f original state ->
      let open Meta.Let_syntax in
      let i_state = t state in
      let%map out_val, i_state = original i_state in
      let o_state = f i_state state in
      (out_val, o_state)
  end

  include Inner
  include Base.Monad.Make2 (Inner)
end

module State_opt = State_t (Core.Option)
module State = State_t (Base.Monad.Ident)
