{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

rule token = parse
    "true"  { TRUE }
  | "false" { FALSE }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LT }
  | space+ { token lexbuf }
  | eof { EOF }
