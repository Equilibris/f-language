type arrow_ty = { i : ty; o : ty }
and applicative_ty = { ty : ty; arg : ty }
and id_ty = string [@@deriving show]
and tuple_ty = ty list

and ty =
  | Id of id_ty
  (* Simple SystemF *)
  | Applicative of applicative_ty
  | TupleTy of tuple_ty
  | Var of string
  | Arrow of arrow_ty
[@@deriving show]

type bind_expr = { name : string; value : expr; within : expr }
and condition_expr = { predicate : expr; t_branch : expr; f_branch : expr }
and call_expr = { callee : expr; arg : expr }
and lambda_expr = { binding : string; content : expr }
and tuple_expr = expr list
and id_expr = string
and constructor_expr = string * expr
and lit_expr = int

and expr =
  | Bind of bind_expr
  | Condition of condition_expr
  | Call of call_expr
  | Lambda of lambda_expr
  | Tuple of tuple_expr
  | Constructor of constructor_expr
  | Id of id_expr
  | Lit of lit_expr
[@@deriving show]

type ty_def = { name : string; constructors : constructor_def list }
and constructor_def = { constructor : string; ty : ty } [@@deriving show]

type decl_stmt = { name : string; expr : expr } [@@deriving show]
type decl_ty_stmt = { name : string; ty : ty } [@@deriving show]

type stmt = Decl of decl_stmt | DeclTy of decl_ty_stmt | TyDef of ty_def
[@@deriving show]
