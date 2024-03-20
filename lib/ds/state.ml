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

    let translate :
        ('old_state -> 'new_value) ->
        ('new_state -> 'old_state -> 'old_state) ->
        ('new_value -> ('new_state * 'value) Meta.t) ->
        ('value, 'old_state) t =
     fun t f original state ->
      let open Meta.Let_syntax in
      let i_state = t state in
      let%map i_state, out_val = original i_state in
      let o_state = f i_state state in
      (o_state, out_val)

    let translate_cont t f original state =
      let open Meta.Let_syntax in
      let%map i_state, out_val = t state original in
      let o_state = f i_state state in
      (o_state, out_val)
  end

  include Inner
  include Base.Monad.Make2 (Inner)
end

(** option state monad *)
module State_opt = struct
  include State_t (Core.Option)

  let fail x = ((fun _ -> None) |> effectless) x
end

module State = State_t (Base.Monad.Ident)
(** Identity state monad *)

(** Some basic setters for operating on hash maps *)
module UtilSetters (M : Base.Monad.S) = struct
  open Core
  open State_t (M)

  let set_key key x =
    translate
      (fun map -> Map.find map key)
      (fun data map ->
        match data with None -> map | Some data -> Map.set map ~key ~data)
      (* (fun map s -> *)
      (*   match s with None -> map | Some data -> Map.set map ~key ~data) *)
      x
end
