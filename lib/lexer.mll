{
    open Parser
}

let digit = ['0'-'9']
let ws =  [' ' '\t' '\n']
let nws = [^ ' ' '\t' '\n']

rule token = parse
    | ws+                  { token lexbuf } (* This is as simple as a recursive call *)
    | digit+ as number     { INT(int_of_string number) }
    | ":="                 { WALRUS }
    | ':'                  { COLON }
    | ','                  { COMMA }
    | '['                  { RBRACKET }
    | ']'                  { LBRACKET }
    | '('                  { RPAREN }
    | ')'                  { LPAREN }
    | "if"                 { IF }
    | "then"               { THEN }
    | "else"               { ELSE }
    | (['A'-'Z']nws*) as s { CONSTRUCTOR(s) }
    | (['a'-'z']nws*) as s { ID(s) }
    | eof                  { EOF }

