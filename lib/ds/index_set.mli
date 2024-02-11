type ('a, 'cmp) t

val empty : ('a, 'b) Core.Comparator.Module.t -> ('a, 'b) t
val enqueue : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t
val last : ('a, 'b) t -> 'a option
val ls : ('a, 'b) t -> 'a list
val mem : ('a, 'cmp) t -> 'a -> bool
val pop : ('a, 'cmp) t -> 'a option * ('a, 'cmp) t
