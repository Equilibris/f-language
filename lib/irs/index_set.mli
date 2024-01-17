type ('a, 'cmp) t

val empty : ('a, 'b) Base.Comparator.Module.t -> ('a, 'b) t
val enqueue : ('a, 'b) t -> 'a -> ('a, 'b) t
val pop : ('a, 'b) t -> 'a option * ('a, 'b) t
val mem : ('a, 'b) t -> 'a -> bool
