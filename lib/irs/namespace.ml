open Core

type ('a, 'b, 'c) t = {
  i2s : (int, 'a, 'b) Base.Map.t;
  s2i : ('a, int, 'c) Base.Map.t;
  req : ('a, int, 'c) Base.Map.t;
  i : int; [@default 0]
}
[@@deriving fields, make]

let string_namespace =
  let i2s = Map.empty (module Int) in
  let s2i = Map.empty (module String) in
  make ~i2s ~s2i ~req:s2i ()

let resolve ({ i2s; s2i; i; req } as id) name =
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

let assign { i2s; s2i; i; req } name =
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

let scopef ns f =
  let nns, v = f ns in
  (scope nns ns, v)

let base_bind { i2s; s2i; i; req } name =
  ( {
      i2s = Map.set ~key:i ~data:name i2s;
      s2i = Map.set ~key:name ~data:i s2i;
      i = i + 1;
      req;
    },
    i )

let bind (ns, name) within =
  let ns, i = base_bind ns name in
  let nns, v = within (ns, i) in
  (scope ns nns, v)

let ( let* ) = bind
let ( let+ ) = scopef
