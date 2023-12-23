{
    open Parser
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n' '\r']
let id = [^ ' ' '\t' '\n' '\r' '\\' '(' ')' '=' ',' ':' ';' '|']+

rule token = parse
  | whitespace+       { token lexbuf }   (* Skip whitespace *)
  | "if"   { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "data" { DATA }
  | "let"  { LET }
  | "in"   { IN }
  | '\\'   { FUN }
  | '('    { LPAR }
  | ')'    { RPAR }
  | '='    { EQ }
  | ','    { COMMA }
  | "::"   { TYPE }
  | '|'    { OR }
  | ';'    { SEMI }
  | '\'' id as id { TYPE_VAR id }
  | "->" { ARR }
  | id as id
    {
        let open Core in
        if String.get id 0 |> Char.is_uppercase then
            CONSTRUCTOR id
        else ID id
    }
  | eof { EOF }
