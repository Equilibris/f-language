open Core

module WeakMat (S : Objects.SemiRing) : sig
  type t

  (* prog *)
  val init : int * int -> S.t -> t

  val element_mod : t -> ((int * int -> S.t -> unit) -> 'a) -> 'a
  (** edit elements *)

  (* Algebra *)

  val transpose : t -> t
  val ( + ) : t -> t -> t option
  val ( * ) : t -> t -> t option
end = struct
  type t = { dim : int * int; els : S.t array }

  let mk_compose (xdim, _) =
    ((fun idx -> (idx % xdim, idx / xdim)), fun (x, y) -> x + (y * xdim))

  let element_mod { dim; els } op =
    let _, recompose = mk_compose dim in
    op (fun p v -> els.(recompose p) <- v)

  let init ((x, y) as dim) el =
    { dim; els = Array.init ~f:(Fn.const el) (x * y) }

  let transpose { dim = (x_dim, y_dim) as dms; els } =
    let decompose, recompose = mk_compose dms in
    {
      dim = (y_dim, x_dim);
      els =
        Array.init
          ~f:(fun i ->
            let x, y = decompose i in
            let i = recompose (x, y) in
            els.(i))
          (x_dim * y_dim);
    }

  let ( + ) a b =
    if Core.Poly.(a.dim = b.dim) then
      Some { dim = a.dim; els = Array.map2_exn ~f:S.( + ) a.els b.els }
    else None

  let ( * ) { dim = (xa_dim, ya_dim) as ad; els = a_els }
      { dim = (xb_dim, yb_dim) as bd; els = b_els } =
    let dim = (ya_dim, xb_dim) in
    let sz = ya_dim * xb_dim in

    let decompose, _ = mk_compose dim in
    let _, a_recompose = mk_compose ad in
    let _, b_recompose = mk_compose bd in

    if xa_dim = yb_dim then
      Some
        {
          dim;
          els =
            Array.init
              ~f:(fun i ->
                let x, y = decompose i in
                Array.init
                  ~f:(fun i ->
                    S.(a_els.(a_recompose (x, i)) * b_els.(b_recompose (i, y))))
                  xa_dim
                |> Array.reduce_exn ~f:S.( + ))
              sz;
        }
    else None
end

module SemiRingMat (S : Objects.Rig) = struct
  include WeakMat (S)

  let id _ = failwith "todo"
end
