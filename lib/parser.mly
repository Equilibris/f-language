%token EOF
%token IF THEN ELSE
%token RPAREN   LPAREN
%token RBRACKET LBRACKET
%token WALRUS COLON
%token COMMA
%token<int> INT
%token<string> CONSTRUCTOR ID

%type<Ast.tuple_expr> inner_tuple

%type<Ast.bind_expr>        bind
%type<Ast.call_expr>        call
%type<Ast.tuple_expr>       tuple
%type<Ast.lambda_expr>      lambda
%type<Ast.condition_expr>   condition
%type<Ast.constructor_expr> constructor

%type<Ast.expr> expr

%start<Ast.top_level_decl list> top_level


%left FUN
%left COND
%left WALRUS
%left LPAREN
%left ID
%left APP

%%

bind:
    name = ID; WALRUS; value = expr; COLON; within = expr {
        let open Ast in
        { name; value; within }
    }

call:
    callee = expr; arg = expr %prec APP {
        let open Ast in
        { callee; arg; }
    }

lambda:
    RBRACKET; binding = ID; LBRACKET; content = expr %prec FUN{
        let open Ast in
        { binding; content }
    }

condition:
    IF; predicate = expr; THEN t_branch = expr; ELSE; f_branch = expr {
        let open Ast in
        { predicate; t_branch; f_branch }
    }

inner_tuple:
    | current = expr; COMMA; prev = inner_tuple { current::prev }
    | expr { [$1] }
    | { [] }

tuple:
    | RPAREN; a = expr; COMMA; sub = inner_tuple; LPAREN { List.rev (a::sub) }
    | RPAREN; LPAREN { [] }

constructor:
    name = CONSTRUCTOR; expr = expr { (name, expr) }

expr:
    | ID           { Id $1 }
    | RPAREN; expr; LPAREN { $2 }
    | bind         { Bind $1 }
    | condition    { Condition $1 }
    (* | call         { Call $1 } *)
    | lambda       { Lambda $1 }
    | tuple        { Tuple (List.rev $1) }
    | constructor  { Constructor $1 }
    | INT          { Lit $1 }

top_level:
    | name = ID; WALRUS; expr = expr; sub = top_level {
        let open Ast in { name; expr; }::sub
    }
    | EOF { [] }
