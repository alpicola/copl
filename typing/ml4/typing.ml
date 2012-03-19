open Syntax

module IntSet = Set.Make (struct type t = int let compare = compare end)

type ty =
  | TyInt
  | TyBool
  | TyVar of int
  | TyList of ty
  | TyFun of ty * ty

let fresh_tyvar =
  let counter = ref 0 in
  fun () -> let n = !counter in counter := n + 1; TyVar n

let rec free_tyvar = function
  | TyInt | TyBool -> IntSet.empty
  | TyVar n -> IntSet.singleton n
  | TyList t -> free_tyvar t
  | TyFun (t1, t2) -> IntSet.union (free_tyvar t1) (free_tyvar t2)

let string_of_ty t =
  let rec range x y = if x = y then [] else x :: range (x + 1) y in
  let vs = free_tyvar t in
  let names = List.combine (IntSet.elements vs)
    (List.map char_of_int (range 97 (97 + IntSet.cardinal vs))) in
  let rec iter p = function
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyVar n -> Printf.sprintf "'%c" (List.assoc n names)
    | TyList t -> iter true t ^ " list"
    | TyFun (t1, t2) ->
        let s = iter true t1 ^ " -> " ^ iter false t2 in
        if p then "(" ^ s ^ ")" else s in
  iter false t

let eqs_of_subst s = 
  List.map (fun (n, t) -> (TyVar n, t)) s

let mono_ty t = (IntSet.empty, t)

let poly_ty t env =
  let vs = List.fold_left (fun fvs (id, (vs, t)) ->
    IntSet.union fvs (IntSet.diff (free_tyvar t) vs)) IntSet.empty env in
  (IntSet.diff (free_tyvar t) vs, t)

let rec substitute s = function
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar n -> (try List.assoc n s with Not_found -> TyVar n)
  | TyList t ->  TyList (substitute s t)
  | TyFun (t1, t2) -> TyFun (substitute s t1, substitute s t2)

let rec unify = function
  | [] -> []
  | (TyInt, TyInt) :: eqs -> unify eqs 
  | (TyBool, TyBool) :: eqs -> unify eqs
  | (TyVar n1, TyVar n2) :: eqs when n1 = n2 -> unify eqs
  | ((TyVar n, t) | (t, TyVar n)) :: eqs
    when not (IntSet.mem n (free_tyvar t)) ->
      let sub = substitute [(n, t)] in 
      let s = unify (List.map (fun (t1, t2) -> (sub t1, sub t2)) eqs) in
      (n, substitute s t) :: s
  | (TyList t1, TyList t2) :: eqs ->
      unify ((t1, t2) :: eqs)
  | (TyFun (t1, t2), TyFun (t3, t4)) :: eqs ->
      unify ((t1, t3) :: (t2, t4) :: eqs)
  | _ -> failwith "unify error"

let rec infer env = function
  | IntLit i -> ([], TyInt)
  | BoolLit b -> ([], TyBool)
  | Nil -> ([], TyList (fresh_tyvar ()))
  | Fun (id, e) ->
      let v = fresh_tyvar () in
      let (s, t) = infer ((id, mono_ty v) :: env) e in
      (s, TyFun (substitute s v, t))
  | Var id ->
      let (ns, t) = List.assoc id env in
      let s = IntSet.fold (fun n s -> (n, fresh_tyvar ()) :: s) ns [] in 
      ([], substitute s t)
  | Op (o, e1, e2) ->
      let (s1, t1) = infer env e1 in
      let (s2, t2) = infer env e2 in
      let eqs = eqs_of_subst (s1 @ s2) in
      (match o with
        | Plus | Minus | Times ->
            (unify ((t1, TyInt) :: (t2, TyInt) :: eqs), TyInt)
        | Lt ->
            (unify ((t1, TyInt) :: (t2, TyInt) :: eqs), TyBool)
        | Cons ->
            let s = unify ((t2, TyList t1) :: eqs) in
            (s, substitute s t2))
  | Let (id, e1, e2) ->
      let (s1, t1) = infer env e1 in
      let sub = substitute s1 in
      let env2 = List.map (fun (id, (vs, t)) -> (id, (vs, sub t))) env in
      let tysc = poly_ty t1 env2 in
      let (s2, t2) = infer ((id, tysc) :: env) e2 in
      let s3 = unify (eqs_of_subst (s1 @ s2)) in
      (s3, substitute s3 t2)
  | LetRec (id1, id2, e1, e2) ->
      let (v1, v2) = (fresh_tyvar (), fresh_tyvar ()) in
      let (s1, t1) = infer ((id1, mono_ty v1) :: (id2, mono_ty v2) :: env) e1 in
      let s2 = unify ((v1, TyFun (v2, t1)) :: eqs_of_subst s1) in
      let sub = substitute s2 in
      let env2 = List.map (fun (id, (vs, t)) -> (id, (vs, sub t))) env in
      let tysc = poly_ty (sub v1) env2 in
      let (s3, t3) = infer ((id1, tysc) :: env) e2 in
      let s4 = unify (eqs_of_subst (s2 @ s3)) in
      (s4, substitute s4 t3)
  | If (e1, e2, e3) ->
      let (s1, t1) = infer env e1 in
      let (s2, t2) = infer env e2 in
      let (s3, t3) = infer env e3 in
      let s4 = unify ((t1, TyBool) :: (t2, t3) :: eqs_of_subst (s1 @ s2 @ s3)) in
      (s4, substitute s4 t2)
  | App (e1, e2) ->
      let (s1, t1) = infer env e1 in
      let (s2, t2) = infer env e2 in
      let v = fresh_tyvar () in
      let s3 = unify ((t1, TyFun (t2, v)) :: eqs_of_subst (s1 @ s2)) in
      (s3, substitute s3 v)
  | Match (e1, e2, id1, id2, e3) ->
      let (s1, t1) = infer env e1 in
      let (s2, t2) = infer env e2 in
      let v = fresh_tyvar () in
      let (s3, t3) = infer ((id1, mono_ty v) :: (id2, mono_ty (TyList v)) :: env) e3 in
      let s4 = unify ((t1, TyList v) :: (t2, t3) :: eqs_of_subst (s1 @ s2 @ s3)) in
      (s4, substitute s4 t2)
