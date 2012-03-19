open Syntax
open Typing

let parse s = Parser.main Lexer.token (Lexing.from_string s)

let _ =
  try
    while true do
      print_string "# ";
      let input = read_line () in
      try 
        print_string ": ";
        let (s, t) = infer [] (parse input) in
        print_endline (string_of_ty t)
      with
        | Parsing.Parse_error ->
            print_endline "syntax error"
        | Failure message ->
            print_endline message
    done
  with
    | End_of_file -> ()
