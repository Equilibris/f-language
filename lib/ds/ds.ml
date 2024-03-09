module Namespace = Namespace

module State_t (Meta : Base.Monad.S) = struct
  module Inner = struct
    open Meta.Let_syntax

    type ('a, 'state) t = 'state -> ('state * 'a) Meta.t

    let return : 'a -> ('a, 'b) t = fun a state -> Meta.return (state, a)
    let i_ret f s = Meta.return (f s)

    let t_return : 'a Meta.t -> ('a, 'state) t =
     fun v state -> Meta.map ~f:(fun v -> (state, v)) v

    let bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t =
     fun t ~f state ->
      let%bind transient_state, a = t state in
      let%map final_state, b = f a transient_state in
      (final_state, b)

    let map = `Define_using_bind
    let inspect state = Meta.return (state, state)

    let effect f state =
      let%map state = f state in
      (state, ())

    let effectless f state =
      let%map v = f state in
      (state, v)

    let id_state state = Meta.return (state, ())
    let assign s _ = Meta.return (s, ())
    let replace f n old = Meta.return (f n old, ())
    let update f s = Meta.return (f s, ())

    let translate t f original state =
      let open Meta.Let_syntax in
      let i_state = t state in
      let%map i_state, out_val = original i_state in
      let o_state = f i_state state in
      (o_state, out_val)
  end

  include Inner
  include Base.Monad.Make2 (Inner)
end

module State_opt = State_t (Core.Option)
module State = State_t (Base.Monad.Ident)
