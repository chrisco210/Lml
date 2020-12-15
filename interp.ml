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

let string_of_env (env : lamcom list) : string =
  "[" 
  ^ (List.fold_left (fun a b -> a ^ "; " ^ (string_of_exp b)) "" env) 
  ^ "]"

(* For now, lets just do call by value, using big step evaluation *)
let rec eval (exp : lamcom) : lamcom = 
  let rec eval_env (env : lamcom list) (exp : lamcom) =
    match exp with
    (* Eval on left *)
    | App (e1, e2) -> let e1' = eval e1 in begin 
        match e1' with 
        | Lam e3 -> eval_env (e2::env) e3
        | _ -> failwith "Cannot apply to a non function."
      end
    | Var n -> begin 
        match List.nth_opt env n with
        | Some e -> e
        | None -> failwith ("Error: Unbound variable: " 
                            ^ (string_of_int n) 
                            ^ " in environment " 
                            ^ (string_of_env env))
      end
    (* Values step to themselves *)

    | Int n -> Int n
    | Bool b -> Bool b
    | Lam e -> Lam e
    | Unit -> Unit
    (* The three additional extensions *)
    | Proj (n, e') -> let v = eval_env env e' in begin match v with 
        | Pair (v1, v2) -> if n = 0 then v1 else v2 
        | _ -> failwith "Invalid tuple projection"
      end
    | Pair (e1, e2) -> Pair (eval_env env e1, eval_env env e2)
    | If (b, etrue, efalse) -> begin 
        match eval_env env b with 
        | Bool true -> eval_env env etrue
        | Bool false -> eval_env env efalse
        | _ -> failwith "Argument mismatch for if statement"
      end
    | Bop (op, l, r) -> 
      let l' = 
        match eval_env env l with 
        | Int n -> n 
        | _ -> failwith ("Binary operators must take integers on lhs: " ^ string_of_exp (Bop (op, l, r))) 
      in
      let r' = 
        match eval_env env r with 
        | Int n -> n 
        | _ -> failwith ("Binary operators must take integers on rhs: " ^ string_of_exp (Bop (op, l, r)))
      in 
      eval_bop op l' r'
    | Uop (Neg, e) ->
      begin 
        match eval_env env e with
        | Int n -> Int (-n)
        | _ -> failwith ("Neg must take an integer: " ^ string_of_exp (Uop (Neg, e)))
      end 
  in 
  eval_env [] exp