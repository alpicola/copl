type op = Plus | Minus | Times | Lt | Cons

type exp =
  | IntLit of int
  | BoolLit of bool
  | Var of string
  | Op of op * exp * exp
  | If of exp * exp * exp
  | Let of string * exp * exp
  | LetRec of string * string * exp * exp
  | Match of exp * exp * string * string * exp
  | Fun of string * exp
  | App of exp * exp
  | Nil
