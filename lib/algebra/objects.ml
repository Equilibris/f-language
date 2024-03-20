module type Monoid = sig
  type t

  val one : t
  val ( * ) : t -> t -> t
end

module type Group = sig
  include Monoid

  val ( / ) : t -> t -> t
end

module type SemiRing = sig
  include Monoid

  val ( + ) : t -> t -> t
end

module type Rig = sig
  include SemiRing

  val zero : t
end

(* Might be wrong but this is fine *)
module type Rng = sig
  include SemiRing

  val ( - ) : t -> t -> t
end
