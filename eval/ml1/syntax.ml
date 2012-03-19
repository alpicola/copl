type op = Plus | Minus | Times | Lt

type exp =
  | IntLit of int
  | BoolLit of bool
  | Op of op * exp * exp
  | If of exp * exp * exp
