open Syntax

type value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of string * exp * env
  | RecFunVal of string * string * exp * env
  | ListVal of value list
and env = (string * value) list

let rec string_of_val = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | FunVal _ -> "fun"
  | RecFunVal _ -> "rec fun"
  | ListVal l ->
      (match l with
        | v :: vs ->
            string_of_val v ^ " :: " ^ string_of_val (ListVal vs)
        | _ -> "[]")

let apply_op o v1 v2 = match o, v1, v2 with
  | Plus, IntVal i1, IntVal i2 -> IntVal (i1 + i2)
  | Minus, IntVal i1, IntVal i2 -> IntVal (i1 - i2)
  | Times, IntVal i1, IntVal i2 -> IntVal (i1 * i2)
  | Lt, IntVal i1, IntVal i2 -> BoolVal (i1 < i2)
  | Cons, (v:value), ListVal vs -> ListVal (v :: vs)
  | _, _, _ -> failwith "type error"

let rec resolve id = function
  | (i, v) :: _ when i = id -> v
  | _ :: e -> resolve id e
  | _ -> failwith "variable resolve error"

let rec eval env = function
  | IntLit i -> IntVal i
  | BoolLit b -> BoolVal b
  | Nil -> ListVal []
  | Fun (id, e) -> FunVal (id, e, env)
  | Var id -> resolve id env
  | Op (o, e1, e2) -> apply_op o (eval env e1) (eval env e2)
  | Let (id, e1, e2) -> eval ((id, eval env e1) :: env) e2
  | LetRec (id1, id2, e1, e2) ->
      eval ((id1, RecFunVal (id1, id2, e1, env)) :: env) e2
  | If (e1, e2, e3) ->
      (match eval env e1 with
        | BoolVal true  -> eval env e2
        | BoolVal false -> eval env e3
        | _ -> failwith "type error")
  | App (e1, e2) ->
      (match eval env e1 with
        | FunVal (id, e, env2) ->
            eval ((id, eval env e2) :: env2) e
        | RecFunVal (id1, id2, e, env2) as f ->
            eval ((id2, eval env e2) :: (id1, f) :: env2) e
        | _ -> failwith "type error")
  | Match (e1, e2, id1, id2, e3) ->
      (match eval env e1 with
        | ListVal [] -> eval env e2
        | ListVal (v :: vs) -> eval ((id1, v) :: (id2, ListVal vs) :: env) e3
        | _ -> failwith "type error")
