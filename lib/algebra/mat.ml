open Core

module WeakMat (S : Objects.SemiRing) = struct
  type t = { dim : int * int; els : S.t array }

  let of_array ((x, y) as dim) els =
    if x * y = Array.length els then Some { dim; els } else None

  let of_list dim els = of_array dim (Array.of_list els)

  let mk_compose (xdim, _) =
    ((fun idx -> (idx % xdim, idx / xdim)), fun (x, y) -> x + (y * xdim))

  let element_mod { dim; els } ~f =
    let _, recompose = mk_compose dim in
    f (fun p v -> els.(recompose p) <- v)

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
              ~f:
                S.(
                  fun i ->
                    let x, y = decompose i in
                    Array.init
                      ~f:(fun i ->
                        a_els.(a_recompose (x, i)) * b_els.(b_recompose (i, y)))
                      xa_dim
                    |> Array.reduce_exn ~f:( + ))
              sz;
        }
    else None

  let is_square { dim = a, b; els = _ } = Int.equal a b

  let ( = ) { dim = xa_dim, ya_dim; els = a_els }
      { dim = xb_dim, yb_dim; els = b_els } =
    Int.(xa_dim = xb_dim && ya_dim = yb_dim)
    && Array.for_all2_exn a_els b_els ~f:S.( = )

  let map ~f { dim; els } = { dim; els = Array.map ~f els }

  let to_string ~to_string { dim = xdim, _; els } =
    Array.mapi
      ~f:(fun i v ->
        let i = i % xdim in
        sprintf "%s%s%s"
          (if Int.(i = 0) then "[ " else ", ")
          (to_string v)
          (if Int.(i = xdim - 1) then " ]\n" else ""))
      els
    |> Array.to_list |> String.concat
end

module RigMat (S : Objects.Rig) = struct
  include WeakMat (S)

  let one sz =
    let dim = (sz, sz) in
    let decompose, _ = mk_compose dim in
    {
      dim;
      els =
        Array.init
          Int.(sz * sz)
          ~f:(fun v ->
            let a, b = decompose v in
            if Int.(a = b) then S.one else S.zero);
    }
end

module RigFixMat (S : Objects.RigFix) = struct
  include RigMat (S)

  (**
    Finds the fix of a given matrix

    # Requirements

    - Equality
    - Multiplication
    - A fix operator

    # The formula

        $$s^* = s \times s^*$$

    # How I came up with this strange idea

    - We assume all matrices of semirings converge to a fixed point
        - This is the only way this function can terminate
    - The fixed points elements are all fixed points of the semiring
    - We cannot 'guess' incorrectly (see guess below)
    - The formula given is a *validating* formula, not a generating one

    Dylan helped me come up with this by telling me my original idea
    was goofy :)
   *)
  let rec fix s =
    let open Option.Let_syntax in
    let guess = map ~f:S.fix in
    let star = guess s in
    let%bind reapp = s * star in
    if reapp = star then return star
    else
      let%bind s = s * s in
      fix s
end
