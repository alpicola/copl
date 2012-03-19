type op = Plus | Minus | Times | Lt

type exp =
  | IntLit of int
  | BoolLit of bool
  | Var of string
  | Op of op * exp * exp
  | If of exp * exp * exp
  | Let of string * exp * exp
