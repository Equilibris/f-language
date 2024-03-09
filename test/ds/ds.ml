module State_t = struct
  open Flang.Ds

  module Op_counting = struct
    module Ident = State_t (Base.Monad.Ident)

    let add n m (ac, mc) = ((ac + 1, mc), n + m)
    let mul n m (ac, mc) = ((ac, mc + 1), n * m)

    let c =
      let open Ident.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = mul v 2 in
      m

    let () = assert (c (0, 0) = ((1, 1), 6))
  end

  module Op_counting_opt = struct
    let add n m (ac, mc) = Some ((ac + 1, mc), n + m)
    let sub n m (ac, mc) = Some ((ac + 1, mc), n - m)
    let mul n m (ac, mc) = Some ((ac, mc + 1), n * m)
    let div n m (ac, mc) = if m = 0 then None else Some ((ac, mc + 1), n / m)

    let c =
      let open State_opt.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = mul v 2 in
      m

    let zd =
      let open State_opt.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = div v 0 in
      m

    let () = assert (c (0, 0) = Some ((1, 1), 6))
    let () = assert (zd (0, 0) = None)
  end

  module Inspection = struct
    open Op_counting_opt

    let c =
      let open State_opt.Let_syntax in
      let%bind v = add 1 2 in
      let%bind curr = State_opt.inspect in
      let%map m = mul v 2 in
      (m, curr)
  end
end
