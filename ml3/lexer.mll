{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']

rule token = parse
    "true"  { TRUE }
  | "false" { FALSE }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "let"   { LET }
  | "rec"   { REC }
  | "in"    { IN }
  | "fun"   { FUN }
  | "->"    { ARROW }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LT }
  | '=' { EQ }
  | alpha (alpha|digit)* { VAR (Lexing.lexeme lexbuf) }
  | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | space+ { token lexbuf }
  | eof { EOF }
