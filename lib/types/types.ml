include Unify
include Canonicalize
include Infer_expr
include Infer_stmt

module Type_map : sig
  type ('a, 'cmp) t = ('a, 'cmp) Type_map.t

  val make :
    var_map:(int, int Irs.Ast.ty, 'a) Core.Map.t ->
    id_ty_map:(int, int Irs.Ast.ty, 'a) Core.Map.t ->
    ?cursor:int ->
    stack:(int, 'b) Ds.Index_set.t ->
    unit ->
    ('a, 'b) t

  val stack : ('a, 'b) t -> (int, 'b) Ds.Index_set.t
  val cursor : ('a, 'b) t -> int
  val id_ty_map : ('a, 'b) t -> (int, int Irs.Ast.ty, 'a) Core.Map.t
  val var_map : ('a, 'b) t -> (int, int Irs.Ast.ty, 'a) Core.Map.t

  module Fields = Type_map.Fields

  val empty : (int, 'a) Core.Comparator.Module.t -> ('a, 'a) t
  val enqueue : ('a, 'b) t -> int -> ('a, 'b) t

  val set_or_update :
    ('a, 'b) t -> int -> Core.Int.t Irs.Ast.ty -> ('a, 'b) t option

  val pop : ('a, 'b) t -> int option * ('a, 'b) t
  val mem : ('a, 'b) t -> int -> bool
  val bind_var : ('a, 'b) t -> ('a, 'b) t * int Irs.Ast.ty
  val mint : ('a, 'b) t -> int -> ('a, 'b) t * int Irs.Ast.ty
end =
  Type_map
