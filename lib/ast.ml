type bind_expr = {
    name : string;
    value : expr;
    within : expr;
}
and condition_expr = {
    predicate : expr;
    t_branch : expr;
    f_branch : expr;
}
and call_expr = {
    callee : expr;
    arg : expr;
}
and lambda_expr = {
    binding: string;
    content: expr;
}
and tuple_expr = expr list
and id_expr = string
and constructor_expr = string * expr
and lit_expr = int
and expr =
    | Bind          of bind_expr
    | Condition     of condition_expr
    | Call          of call_expr
    | Lambda        of lambda_expr
    | Tuple         of tuple_expr
    | Constructor   of constructor_expr
    | Id            of id_expr
    | Lit           of lit_expr
    [@@deriving show]

type top_level_decl = {
    name : string;
    expr : expr;
} [@@deriving show]

