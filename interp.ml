open Lambdaast

(** [shift i c e] Implements the raising function for handling free variables 
    de Bruijn LC
*)
let rec shift (i : var) (c : int) (e : lamcom) : lamcom =
  match e with
  | App (e1, e2) -> App (shift i c e1, shift i c e2)
  | Lam e -> Lam (shift i (c + 1) e)
  | Var n -> if (n < c) then Var n else Var (n + i)
  | Int n -> Int n

(** [sub e1 e2 n] substitutes [e2] for [n] in [e1]*)
let rec sub (e1 : lamcom) (e2 : lamcom) (m : var) = 
  match e1 with 
  | App (el, er) -> App ((sub el e2 m), (sub er e2 m))
  | Lam e -> Lam (sub e (shift 1 0 e2) (m + 1))
  | Var n -> if n = m then e2 else Var n
  | Int n -> Int n

let is_val (exp : lamcom) : bool =
  match exp with 
  | App _ -> false
  | _ -> true

(* For now, lets just do call by value *)
let rec eval (exp : lamcom) : lamcom = 
  match exp with
  (* Beta rule *)
  | App (Lam e1, e2) when is_val e2-> shift ~-1 0 (eval (sub e1 (shift 1 0 e2) 0))
  | App (Lam e1, e2) -> let e2' = eval e2 in eval (App (Lam e1, e2'))
  | App (e1, e2) -> let e1' = eval e1 in eval (App (e1', e2))
  | Var n -> Var n
  | Int n -> Int n
  | _ -> failwith "Unimplemented"