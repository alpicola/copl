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

let rec resolve id = function
  | (i, v) :: _ when i = id -> v
  | _ :: e -> resolve id e
  | _ -> failwith "variable resolve error"

let rec eval env = function
  | IntLit i -> IntVal i
  | BoolLit b -> BoolVal b
  | Var id -> resolve id env
  | Op (o, e1, e2) -> apply_op o (eval env e1) (eval env e2)
  | Let (id, e1, e2) -> eval ((id, eval env e1) :: env) e2
  | If (e1, e2, e3) ->
      (match eval env e1 with
        | BoolVal true  -> eval env e2
        | BoolVal false -> eval env e3
        | _ -> failwith "type error")
