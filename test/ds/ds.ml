module State_t = struct
  open Flang.Ds

  module Op_counting = struct
    module Ops_counts =
      State_t
        (struct
          type t = int * int
        end)
        (Base.Monad.Ident)

    let add n m (ac, mc) = (n + m, (ac + 1, mc))
    let mul n m (ac, mc) = (n * m, (ac, mc + 1))

    let c =
      let open Ops_counts.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = mul v 2 in
      m

    let () = assert (c (0, 0) = (6, (1, 1)))
  end

  module Op_counting_opt = struct
    module Ops_counts =
      State_t
        (struct
          type t = int * int
        end)
        (Core.Option)

    let add n m (ac, mc) = Some (n + m, (ac + 1, mc))
    let sub n m (ac, mc) = Some (n - m, (ac + 1, mc))
    let mul n m (ac, mc) = Some (n * m, (ac, mc + 1))
    let div n m (ac, mc) = if m = 0 then None else Some (n / m, (ac, mc + 1))

    let c =
      let open Ops_counts.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = mul v 2 in
      m

    let zd =
      let open Ops_counts.Let_syntax in
      let%bind v = add 1 2 in
      let%map m = div v 0 in
      m

    let () = assert (c (0, 0) = Some (6, (1, 1)))
    let () = assert (zd (0, 0) = None)
  end

  module Inspection = struct
    open Op_counting_opt

    let c =
      let open Ops_counts.Let_syntax in
      let%bind v = add 1 2 in
      let%bind curr = Ops_counts.inspect in
      let%map m = mul v 2 in
      (m, curr)
  end
end
