open Core

type ('key, 'i_cmp, 'k_cmp) t = {
  i2s : (int, 'key, 'i_cmp) Base.Map.t;
  s2i : ('key, int, 'k_cmp) Base.Map.t;
  req : ('key, int, 'k_cmp) Base.Map.t;
  i : int; [@default 0]
}
[@@deriving fields, make]

let string_namespace =
  let i2s = Map.empty (module Int) in
  let s2i = Map.empty (module String) in
  make ~i2s ~s2i ~req:s2i ()

let resolve name ({ i2s; s2i; i; req } as id) =
  match (Map.find s2i name, Map.find req name) with
  | None, Some v -> (id, v)
  | None, None ->
      ( {
          i2s = Map.set ~key:i ~data:name i2s;
          i = i + 1;
          req = Map.set ~key:name ~data:i req;
          s2i;
        },
        i )
  | Some v, _ -> (id, v)

let assign name { i2s; s2i; i; req } =
  match Map.find req name with
  | Some v ->
      ( {
          i2s = Map.set ~key:v ~data:name i2s;
          s2i = Map.set ~key:name ~data:v s2i;
          i;
          req = Map.remove req name;
        },
        v )
  | None ->
      ( {
          i2s = Map.set ~key:i ~data:name i2s;
          s2i = Map.set ~key:name ~data:i s2i;
          i = i + 1;
          req = Map.remove req name;
        },
        i )

let scope { s2i; i2s = _; i = _; req = _ } ns = { ns with s2i }

let scopef f ns =
  let nns, v = f ns in
  (scope ns nns, v)

(** Unsafely binds the key  *)
let bind name { i2s; s2i; i; req } =
  ( {
      i2s = Map.set ~key:i ~data:name i2s;
      s2i = Map.set ~key:name ~data:i s2i;
      i = i + 1;
      req;
    },
    i )

(* Can de defined on state_T *)
(* let ( let+ ) a b = scopef *)
