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
  | "type" { TYPE }
  | "let"  { LET }
  | "in"   { IN }
  | '\\'   { FUN }
  | '('    { LPAR }
  | ')'    { RPAR }
  | '='    { EQ }
  | ','    { COMMA }
  | "::"   { SPEC_TYPE }
  | '|'    { OR }
  | ';'    { SEMI }
  | '\'' id as id { TYPE_VAR (String.to_seq id |> Seq.drop 1 |> String.of_seq) }
  | "->" { ARR }
  | id as id
    {
        let open Core in
        if String.get id 0 |> Char.is_uppercase then
            CONSTRUCTOR id
        else ID id
    }
  | eof { EOF }
