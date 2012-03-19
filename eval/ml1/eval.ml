open Syntax

type value =
  | IntVal of int
  | BoolVal of bool

let string_of_val = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b

let apply_op o v1 v2 = match o, v1, v2 with
  | Plus, IntVal i1, IntVal i2 -> IntVal (i1 + i2)
  | Minus, IntVal i1, IntVal i2 -> IntVal (i1 - i2)
  | Times, IntVal i1, IntVal i2 -> IntVal (i1 * i2)
  | Lt, IntVal i1, IntVal i2 -> BoolVal (i1 < i2)
  | _, _, _ -> failwith "type error"

let rec eval = function
  | IntLit i -> IntVal i
  | BoolLit b -> BoolVal b
  | Op (o, e1, e2) -> apply_op o (eval e1) (eval e2)
  | If (e1, e2, e3) ->
      (match eval e1 with
        | BoolVal true  -> eval e2
        | BoolVal false -> eval e3
        | _ -> failwith "type error")
