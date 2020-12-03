open Lambdaast
open Pprint

(* This interpreter uses de bruijn notation as it is easier *)

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
  | Bop (op, l, r) -> Bop (op, shift i c l, shift i c r)
  | Uop (op, e) -> Uop (op, shift i c e)
  | If (b, etrue, efalse) -> If (shift i c b, shift i c etrue, shift i c efalse)

(** [sub e1 e2 n] substitutes [e2] for [n] in [e1]*)
let rec sub (e1 : lamcom) (e2 : lamcom) (m : var) : lamcom = 
  match e1 with 
  | App (el, er) -> App ((sub el e2 m), (sub er e2 m))
  | Lam e -> Lam (sub e (shift 1 0 e2) (m + 1))
  | Var n -> if n = m then e2 else Var n
  | Int n -> Int n
  | Bool b -> Bool b
  | If (b, etrue, efalse) -> If ((sub b e2 m), (sub etrue e2 m), (sub efalse e2 m))
  | Bop (op, l, r) -> Bop (op, sub l e2 m, sub r e2 m)
  | Uop (op, e) -> Uop (op, sub e e2 m)


let is_val (exp : lamcom) : bool =
  match exp with 
  | App _ | If _ | Bop _ -> false
  | _ -> true

(* For now, lets just do call by value, using big step evaluation *)
let rec eval (exp : lamcom) : lamcom = 
  match exp with
  (* Beta rule *)
  | App (Lam e1, e2) -> shift ~-1 0 (eval (sub e1 (shift 1 0 e2) 0))
  (* Eval on left *)
  | App (e1, e2) -> let e1' = eval e1 in eval (App (e1', e2))
  (* Values step to themselves *)
  | Var n -> Var n
  | Int n -> Int n
  | Bool b -> Bool b
  | Lam e -> Lam e
  (* The two additional extensions *)
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
    begin
      match op with 
      | Plus -> Int (l' + r')
      | Minus -> Int (l' - r')
      | Times -> Int (l' * r')
      | Equals -> Bool (l' = r')
      | Lteq -> Bool (l' <= r')
      | Lt -> Bool (l' < r')
      | Gteq -> Bool (l' >= r')
      | Gt -> Bool (l' > r')
      | Neq -> Bool (l' <> r')
    end 
  | Uop (op, e) ->
    begin 
      match op with
      | Not -> begin
          match eval e with
          | Bool b -> Bool (Bool.not b)
          | _ -> failwith ("Not must take a boolean: " ^ string_of_exp (Uop (op, e)))
        end
      | Neg -> begin
          match eval e with
          | Int n -> Int (-n)
          | _ -> failwith ("Neg must take an integer: " ^ string_of_exp (Uop (op, e)))
        end
      | Deref -> failwith "unimplemented in gamma"
    end