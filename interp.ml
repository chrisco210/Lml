open Lambdaast
open Pprint

(* This interpreter uses de bruijn notation as it is easier *)

let eval_bop (b : bop) (l : int) (r : int) : lamcom =
  match b with 
  | Plus -> Int (l + r)
  | Minus -> Int (l - r)
  | Times -> Int (l * r)
  | Equals -> Bool (l = r)
  | Lteq -> Bool (l <= r)
  | Lt -> Bool (l < r)
  | Gteq -> Bool (l >= r)
  | Gt -> Bool (l > r)
  | Neq -> Bool (l <> r)
  | Div -> Int (l / r)

(** [shift i c e] Implements the raising function for handling free variables 
    de Bruijn LC
*)
let rec shift (i : var) (c : int) (e : lamcom) : lamcom =
  match e with
  | App (e1, e2) -> App (shift i c e1, shift i c e2)
  | Lam e -> Lam (shift i (c + 1) e)
  | Var n -> if (n < c) then Var n else Var (n + i)
  | Int n -> Int n
  | Bool b -> Bool b
  | Unit -> Unit
  | Bop (op, l, r) -> Bop (op, shift i c l, shift i c r)
  | Uop (op, e) -> Uop (op, shift i c e)
  | If (b, etrue, efalse) -> If (shift i c b, shift i c etrue, shift i c efalse)
  | Pair (e1, e2) -> Pair (shift i c e1, shift i c e2)
  | Proj (n, e') -> Proj(n, shift i c e')

(** [sub e1 e2 n] substitutes [e2] for [n] in [e1]*)
let rec sub (e1 : lamcom) (e2 : lamcom) (m : var) : lamcom = 
  match e1 with 
  | App (el, er) -> App ((sub el e2 m), (sub er e2 m))
  | Lam e -> Lam (sub e (shift 1 0 e2) (m + 1))
  | Var n -> if n = m then e2 else Var n
  | Int n -> Int n
  | Bool b -> Bool b
  | Unit -> Unit
  | If (b, etrue, efalse) -> If ((sub b e2 m), (sub etrue e2 m), (sub efalse e2 m))
  | Bop (op, l, r) -> Bop (op, sub l e2 m, sub r e2 m)
  | Uop (op, e) -> Uop (op, sub e e2 m)
  | Pair (e, e') -> Pair(sub e e2 m, sub e' e2 m)
  | Proj (n, e') -> Proj(n, sub e' e2 m)

(* For now, lets just do call by value, using big step evaluation *)
let rec eval (exp : lamcom) : lamcom = 
  match exp with
  (* Eval on left *)
  | App (e1, e2) -> let e1' = eval e1 in begin 
      match e1' with 
      | Lam e3 -> shift ~-1 0 (eval (sub e3 (shift 1 0 e2) 0))
      | _ -> failwith "Cannot apply to a non function."
    end
  (* Values step to themselves *)
  | Var n -> Var n
  | Int n -> Int n
  | Bool b -> Bool b
  | Lam e -> Lam e
  | Unit -> Unit
  (* The three additional extensions *)
  | Proj (n, e') -> let v = eval e' in begin match v with 
      | Pair (v1, v2) -> if n = 0 then v1 else v2 
      | _ -> failwith "Invalid tuple projection"
    end
  | Pair (e1, e2) -> Pair (eval e1, eval e2)
  | If (b, etrue, efalse) -> begin 
      match eval b with 
      | Bool true -> eval etrue
      | Bool false -> eval efalse
      | _ -> failwith "Argument mismatch for if statement"
    end
  | Bop (op, l, r) -> 
    let l' = 
      match eval l with 
      | Int n -> n 
      | _ -> failwith ("Binary operators must take integers on lhs: " ^ string_of_exp (Bop (op, l, r))) 
    in
    let r' = 
      match eval r with 
      | Int n -> n 
      | _ -> failwith ("Binary operators must take integers on rhs: " ^ string_of_exp (Bop (op, l, r)))
    in 
    eval_bop op l' r'
  | Uop (Neg, e) ->
    begin 
      match eval e with
      | Int n -> Int (-n)
      | _ -> failwith ("Neg must take an integer: " ^ string_of_exp (Uop (Neg, e)))
    end