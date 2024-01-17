%token LET IN
%token MATCH WITH
%token LPAR RPAR COMMA
%token FUN EQ
%token ARR TYPE SPEC_TYPE OR SEMI
%token EOF

%token <string> ID CONSTRUCTOR TYPE_VAR

%type <unit> maybe_or

%type <(string Irs.Ast.constructor_def) list> constructors

%type <string list> n_ty_vars
%type <string Irs.Ast.tuple_ty> ty_tuple_inner
%type <string Irs.Ast.ty> ty ty_factor
%type <string Irs.Ast.pat_arm list> match_inner

%type<string Irs.Ast.pat list> pat_tuple_inner pat_tuple
%type<string Irs.Ast.pat> pat

%type  <string Irs.Ast.expr> expr expr_atom

%type <string Irs.Ast.tuple_expr> tuple_inner

%start <string Irs.Ast.ty> test_ty
%start <string Irs.Ast.pat> test_pat
%start <string Irs.Ast.stmt list> top_level
%start <string Irs.Ast.expr> test_expr

%left FUN
%left LET
%left LPAR
%left ID
%left APP

%right ARR
%left TAPP

%%

ty_tuple_inner:
    | v = ty; COMMA; sub = ty_tuple_inner { v :: sub }
    | ty { [$1] }
    | { [] }

ty_factor:
    | LPAR; ty; RPAR
        { $2 }
    | LPAR; RPAR
        { Irs.Ast.(TupleTy []) }
    | LPAR; v = ty; COMMA; sub = ty_tuple_inner RPAR
        { Irs.Ast.(TupleTy (v::sub)) }
    | ID
        { Irs.Ast.(Id $1)}
    | TYPE_VAR
        { Irs.Ast.(Var $1) }
    | ty = ty_factor; arg = ty_factor %prec TAPP
        { Irs.Ast.(Applicative { ty; arg }) }
ty:
    | ty_factor { $1 }
    | i = ty; ARR; o = ty
        { let open Irs.Ast in Arrow { i; o } }

pat_tuple_inner:
    | pat; COMMA; pat_tuple_inner { $1 :: $3 }
    | pat { [$1] }
    | { [] }

pat_tuple:
    | pat; COMMA; pat_tuple_inner { $1 :: $3 }
    | { [] }

pat:
    | LPAR; pat; RPAR { $2 }
    | LPAR; pat_tuple; RPAR { Irs.Ast.(TuplePat ($2)) }
    | CONSTRUCTOR; pat { Irs.Ast.(ConstructorPat($1, $2)) }
    | ID { Irs.Ast.(BindingPat $1) }

n_ty_vars:
    | TYPE_VAR; n_ty_vars { $1 :: $2 }
    | { [] }

constructors:
    | constructor = CONSTRUCTOR; ty = ty
        { Irs.Ast.([{ constructor; ty }]) }
    | constructor = CONSTRUCTOR; ty = ty; OR; sub = constructors
        { Irs.Ast.({ constructor; ty }::sub) }

tuple_inner:
    | v = expr; COMMA; sub = tuple_inner { v :: sub }
    | expr { [$1] }
    | { [] }

match_inner:
    | OR; pat; ARR; expr; match_inner { ($2, $4) :: $5 }
    | { [] }

(* TODO: Fix precidence *)
expr_atom:
    | LPAR; RPAR
        { Irs.Ast.(Tuple []) }
    | LPAR; expr; RPAR
        { $2 }
    | ID { Irs.Ast.(Id $1) }
    | MATCH; expr; WITH; maybe_or; pat; ARR; expr; match_inner
        { Irs.Ast.(Match ($2, ($5, $7) :: $8)) }

expr:
    | id = CONSTRUCTOR; expr = expr
        { Irs.Ast.(Constructor (id, expr)) }
    | LPAR; fst = expr; COMMA; cont = tuple_inner; RPAR
        { Irs.Ast.(Tuple (fst :: cont)) }
    | LET; name = ID; EQ; value = expr; IN; within = expr;
        { Irs.Ast.(Bind { name; value; within }) }
    | FUN; binding = ID; content = expr %prec FUN
        { Irs.Ast.(Lambda { binding; content }) }
    | callee = expr; arg = expr %prec APP
        { Irs.Ast.(Call { callee; arg }) }
    | expr_atom { $1 }

test_pat:
    pat; EOF { $1 }

test_ty:
    ty; EOF { $1 }

test_expr:
    expr; EOF { $1 }

maybe_or:
    | OR { () }
    | { () }

top_level:
    | TYPE; name = ID; vars = n_ty_vars; EQ; maybe_or; constructors = constructors; SEMI; sub = top_level
        { Irs.Ast.((TyDef { name; vars; constructors })::sub) }
    | name = ID; SPEC_TYPE; ty = ty; SEMI; sub = top_level
        { Irs.Ast.((DeclTy { name; ty })::sub) }
    | name = ID; EQ; expr = expr; SEMI; sub = top_level {
        Irs.Ast.((Decl { name; expr })::sub)
    }
    | EOF { [] }
