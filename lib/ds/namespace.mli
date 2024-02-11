type ('key, 'i_cmp, 'k_cmp) t

val make :
  i2s:(int, 'a, 'b) Base.Map.t ->
  s2i:('a, int, 'c) Base.Map.t ->
  req:('a, int, 'c) Base.Map.t ->
  ?i:int ->
  unit ->
  ('a, 'b, 'c) t

val i : ('a, 'b, 'c) t -> int
val req : ('a, 'b, 'c) t -> ('a, int, 'c) Base.Map.t
val s2i : ('a, 'b, 'c) t -> ('a, int, 'c) Base.Map.t
val i2s : ('a, 'b, 'c) t -> (int, 'a, 'b) Base.Map.t

module Fields : sig
  val names : string list

  val i :
    ([< `Read | `Set_and_create ], ('a, 'b, 'c) t, int) Base.Field.t_with_perm

  val req :
    ( [< `Read | `Set_and_create ],
      ('a, 'b, 'c) t,
      ('a, int, 'c) Base.Map.t )
    Base.Field.t_with_perm

  val s2i :
    ( [< `Read | `Set_and_create ],
      ('a, 'b, 'c) t,
      ('a, int, 'c) Base.Map.t )
    Base.Field.t_with_perm

  val i2s :
    ( [< `Read | `Set_and_create ],
      ('a, 'b, 'c) t,
      (int, 'a, 'b) Base.Map.t )
    Base.Field.t_with_perm

  val make_creator :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      'd ->
      ('e -> (int, 'f, 'g) Base.Map.t) * 'h) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('i, 'j, 'k) t,
         ('i, int, 'k) Base.Map.t )
       Base.Field.t_with_perm ->
      'h ->
      ('e -> ('f, int, 'l) Base.Map.t) * 'm) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('n, 'o, 'p) t,
         ('n, int, 'p) Base.Map.t )
       Base.Field.t_with_perm ->
      'm ->
      ('e -> ('f, int, 'l) Base.Map.t) * 'q) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('r, 's, 't) t,
         int )
       Base.Field.t_with_perm ->
      'q ->
      ('e -> int) * 'u) ->
    'd ->
    ('e -> ('f, 'g, 'l) t) * 'u

  val create :
    i2s:(int, 'a, 'b) Base.Map.t ->
    s2i:('a, int, 'c) Base.Map.t ->
    req:('a, int, 'c) Base.Map.t ->
    i:int ->
    ('a, 'b, 'c) t

  val map :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      (int, 'd, 'e) Base.Map.t) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('f, 'g, 'h) t,
         ('f, int, 'h) Base.Map.t )
       Base.Field.t_with_perm ->
      ('d, int, 'i) Base.Map.t) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('j, 'k, 'l) t,
         ('j, int, 'l) Base.Map.t )
       Base.Field.t_with_perm ->
      ('d, int, 'i) Base.Map.t) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('m, 'n, 'o) t,
         int )
       Base.Field.t_with_perm ->
      int) ->
    ('d, 'e, 'i) t

  val iter :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      unit) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('d, 'e, 'f) t,
         ('d, int, 'f) Base.Map.t )
       Base.Field.t_with_perm ->
      unit) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('g, 'h, 'i) t,
         ('g, int, 'i) Base.Map.t )
       Base.Field.t_with_perm ->
      unit) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('j, 'k, 'l) t,
         int )
       Base.Field.t_with_perm ->
      unit) ->
    unit

  val fold :
    init:'a ->
    i2s:
      ('a ->
      ( [< `Read | `Set_and_create ],
        ('b, 'c, 'd) t,
        (int, 'b, 'c) Base.Map.t )
      Base.Field.t_with_perm ->
      'e) ->
    s2i:
      ('e ->
      ( [< `Read | `Set_and_create ],
        ('f, 'g, 'h) t,
        ('f, int, 'h) Base.Map.t )
      Base.Field.t_with_perm ->
      'i) ->
    req:
      ('i ->
      ( [< `Read | `Set_and_create ],
        ('j, 'k, 'l) t,
        ('j, int, 'l) Base.Map.t )
      Base.Field.t_with_perm ->
      'm) ->
    i:
      ('m ->
      ([< `Read | `Set_and_create ], ('n, 'o, 'p) t, int) Base.Field.t_with_perm ->
      'q) ->
    'q

  val map_poly :
    ([< `Read | `Set_and_create ], ('a, 'b, 'c) t, 'd) Base.Field.user ->
    'd list

  val for_all :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('d, 'e, 'f) t,
         ('d, int, 'f) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('g, 'h, 'i) t,
         ('g, int, 'i) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('j, 'k, 'l) t,
         int )
       Base.Field.t_with_perm ->
      bool) ->
    bool

  val exists :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('d, 'e, 'f) t,
         ('d, int, 'f) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('g, 'h, 'i) t,
         ('g, int, 'i) Base.Map.t )
       Base.Field.t_with_perm ->
      bool) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('j, 'k, 'l) t,
         int )
       Base.Field.t_with_perm ->
      bool) ->
    bool

  val to_list :
    i2s:
      (( [< `Read | `Set_and_create ],
         ('a, 'b, 'c) t,
         (int, 'a, 'b) Base.Map.t )
       Base.Field.t_with_perm ->
      'd) ->
    s2i:
      (( [< `Read | `Set_and_create ],
         ('e, 'f, 'g) t,
         ('e, int, 'g) Base.Map.t )
       Base.Field.t_with_perm ->
      'd) ->
    req:
      (( [< `Read | `Set_and_create ],
         ('h, 'i, 'j) t,
         ('h, int, 'j) Base.Map.t )
       Base.Field.t_with_perm ->
      'd) ->
    i:
      (( [< `Read | `Set_and_create ],
         ('k, 'l, 'm) t,
         int )
       Base.Field.t_with_perm ->
      'd) ->
    'd list

  module Direct : sig
    val iter :
      ('a, 'b, 'c) t ->
      i2s:
        (( [< `Read | `Set_and_create ],
           ('d, 'e, 'f) t,
           (int, 'd, 'e) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        unit) ->
      s2i:
        (( [< `Read | `Set_and_create ],
           ('g, 'h, 'i) t,
           ('g, int, 'i) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        unit) ->
      req:
        (( [< `Read | `Set_and_create ],
           ('j, 'k, 'l) t,
           ('j, int, 'l) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        unit) ->
      i:
        (( [< `Read | `Set_and_create ],
           ('m, 'n, 'o) t,
           int )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        'p) ->
      'p

    val fold :
      ('a, 'b, 'c) t ->
      init:'d ->
      i2s:
        ('d ->
        ( [< `Read | `Set_and_create ],
          ('e, 'f, 'g) t,
          (int, 'e, 'f) Base.Map.t )
        Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        'h) ->
      s2i:
        ('h ->
        ( [< `Read | `Set_and_create ],
          ('i, 'j, 'k) t,
          ('i, int, 'k) Base.Map.t )
        Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        'l) ->
      req:
        ('l ->
        ( [< `Read | `Set_and_create ],
          ('m, 'n, 'o) t,
          ('m, int, 'o) Base.Map.t )
        Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        'p) ->
      i:
        ('p ->
        ( [< `Read | `Set_and_create ],
          ('q, 'r, 's) t,
          int )
        Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        't) ->
      't

    val for_all :
      ('a, 'b, 'c) t ->
      i2s:
        (( [< `Read | `Set_and_create ],
           ('d, 'e, 'f) t,
           (int, 'd, 'e) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        bool) ->
      s2i:
        (( [< `Read | `Set_and_create ],
           ('g, 'h, 'i) t,
           ('g, int, 'i) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        bool) ->
      req:
        (( [< `Read | `Set_and_create ],
           ('j, 'k, 'l) t,
           ('j, int, 'l) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        bool) ->
      i:
        (( [< `Read | `Set_and_create ],
           ('m, 'n, 'o) t,
           int )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        bool) ->
      bool

    val exists :
      ('a, 'b, 'c) t ->
      i2s:
        (( [< `Read | `Set_and_create ],
           ('d, 'e, 'f) t,
           (int, 'd, 'e) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        bool) ->
      s2i:
        (( [< `Read | `Set_and_create ],
           ('g, 'h, 'i) t,
           ('g, int, 'i) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        bool) ->
      req:
        (( [< `Read | `Set_and_create ],
           ('j, 'k, 'l) t,
           ('j, int, 'l) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        bool) ->
      i:
        (( [< `Read | `Set_and_create ],
           ('m, 'n, 'o) t,
           int )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        bool) ->
      bool

    val to_list :
      ('a, 'b, 'c) t ->
      i2s:
        (( [< `Read | `Set_and_create ],
           ('d, 'e, 'f) t,
           (int, 'd, 'e) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        'g) ->
      s2i:
        (( [< `Read | `Set_and_create ],
           ('h, 'i, 'j) t,
           ('h, int, 'j) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        'g) ->
      req:
        (( [< `Read | `Set_and_create ],
           ('k, 'l, 'm) t,
           ('k, int, 'm) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        'g) ->
      i:
        (( [< `Read | `Set_and_create ],
           ('n, 'o, 'p) t,
           int )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        'g) ->
      'g list

    val map :
      ('a, 'b, 'c) t ->
      i2s:
        (( [< `Read | `Set_and_create ],
           ('d, 'e, 'f) t,
           (int, 'd, 'e) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        (int, 'a, 'b) Base.Map.t ->
        (int, 'g, 'h) Base.Map.t) ->
      s2i:
        (( [< `Read | `Set_and_create ],
           ('i, 'j, 'k) t,
           ('i, int, 'k) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        ('g, int, 'l) Base.Map.t) ->
      req:
        (( [< `Read | `Set_and_create ],
           ('m, 'n, 'o) t,
           ('m, int, 'o) Base.Map.t )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        ('a, int, 'c) Base.Map.t ->
        ('g, int, 'l) Base.Map.t) ->
      i:
        (( [< `Read | `Set_and_create ],
           ('p, 'q, 'r) t,
           int )
         Base.Field.t_with_perm ->
        ('a, 'b, 'c) t ->
        int ->
        int) ->
      ('g, 'h, 'l) t

    val set_all_mutable_fields : 'a -> unit
  end
end

val string_namespace :
  (string, Base.Int.comparator_witness, Base.String.comparator_witness) t

val resolve : ('a, 'b, 'c) t -> 'a -> ('a, 'b, 'c) t * int
val assign : ('a, 'b, 'c) t -> 'a -> ('a, 'b, 'c) t * int
val scope : ('a, 'b, 'c) t -> ('a, 'd, 'c) t -> ('a, 'd, 'c) t

val scopef :
  ('a, 'b, 'c) t ->
  (('a, 'b, 'c) t -> ('a, 'b, 'c) t * 'e) ->
  ('a, 'b, 'c) t * 'e

val bind : ('a, 'b, 'c) t -> 'a -> ('a, 'b, 'c) t * int
(** Unsafely binds the key to namespace without respecting scope *)

val ( let+ ) :
  ('a, 'b, 'c) t ->
  (('a, 'b, 'c) t -> ('a, 'b, 'c) t * 'e) ->
  ('a, 'b, 'c) t * 'e
