%token LET IN
%token IF THEN ELSE
%token LPAR RPAR COMMA
%token FUN EQ
%token ARR TYPE SPEC_TYPE OR SEMI
%token EOF

%token <string> ID CONSTRUCTOR TYPE_VAR

%type <unit> maybe_or

%type <(string Ast.constructor_def) list> constructors

%type <string list> n_ty_vars
%type <string Ast.tuple_ty> ty_tuple_inner
%type <string Ast.ty> ty ty_factor

%type  <string Ast.expr> expr

%type <string Ast.tuple_expr> tuple_inner

%start <string Ast.ty> test_ty
%start <string Ast.stmt list> top_level
%start <string Ast.expr> test_expr

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
        { let open Ast in TupleTy [] }
    | LPAR; v = ty; COMMA; sub = ty_tuple_inner RPAR
        { let open Ast in TupleTy (v::sub) }
    | ID
        { let open Ast in Id $1}
    | TYPE_VAR
        { let open Ast in Var $1 }
    | ty = ty_factor; arg = ty_factor %prec TAPP
        { let open Ast in Applicative { ty; arg } }
ty:
    | ty_factor { $1 }
    | i = ty; ARR; o = ty
        { let open Ast in Arrow { i; o } }

n_ty_vars:
    | TYPE_VAR; n_ty_vars { $1 :: $2 }
    | { [] }

constructors:
    | constructor = CONSTRUCTOR; ty = ty
        { let open Ast in [{ constructor; ty }] }
    | constructor = CONSTRUCTOR; ty = ty; OR; sub = constructors
        { let open Ast in { constructor; ty }::sub }

tuple_inner:
    | v = expr; COMMA; sub = tuple_inner { v :: sub }
    | expr { [$1] }
    | { [] }

expr:
    | id = CONSTRUCTOR; expr = expr
        { let open Ast in Constructor (id, expr) }
    | IF; predicate = expr; THEN; t_branch = expr; ELSE; f_branch = expr
        { let open Ast in Condition { predicate; t_branch; f_branch; } }
    | LPAR; RPAR
        { let open Ast in Tuple [] }
    | LPAR; fst = expr; COMMA; cont = tuple_inner; RPAR
        { let open Ast in Tuple (fst :: cont) }
    | LPAR; expr; RPAR
        { $2 }
    | LET; name = ID; EQ; value = expr; IN; within = expr;
        { let open Ast in Bind { name; value; within } }
    | FUN; binding = ID; content = expr %prec FUN
        { let open Ast in Lambda { binding; content } }
    | callee = expr; arg = expr %prec APP
        { let open Ast in Call { callee; arg } }
    | ID
        { let open Ast in Id $1 }

test_ty:
    ty; EOF { $1 }

test_expr:
    expr; EOF { $1 }

maybe_or:
    | OR { () }
    | { () }

top_level:
    | TYPE; name = ID; vars = n_ty_vars; EQ; maybe_or; constructors = constructors; SEMI; sub = top_level
        { let open Ast in (TyDef { name; vars; constructors })::sub}
    | name = ID; SPEC_TYPE; ty = ty; SEMI; sub = top_level
        { let open Ast in (DeclTy { name; ty })::sub }
    | name = ID; EQ; expr = expr; SEMI; sub = top_level {
        let open Ast in (Decl { name; expr })::sub
    }
    | EOF { [] }
