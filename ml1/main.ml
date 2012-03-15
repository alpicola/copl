open Syntax
open Eval
open Printf

let parse s = Parser.main Lexer.token (Lexing.from_string s)

let _ =
  while true do
    print_string "# ";
    let input = read_line () in
    try 
      printf "=> %s\n" (string_of_val (eval (parse input)))
    with
    | Parsing.Parse_error ->
        print_endline "syntax error"
    | Failure message ->
        print_endline message
  done
