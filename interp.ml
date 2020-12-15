open Lambdaast
open Pprint

(* This, new and improved interpreter uses the environment model, because it is
   faster.  Inspired by the one from assignment 6, but still written on our own.
*)

type value = 
  | VInt of int
  | VBool of bool 
  | VUnit 
  | Closure of lamcom * env 
and env = value list

let expr_of_value e = 
  match e with 
  | VInt n -> Int n
  | VBool b -> Bool b
  | VUnit -> Unit 
  | Closure (b, _) -> Lam b
(* This interpreter uses de bruijn notation as it is easier *)
let eval_bop (b : bop) (l : int) (r : int) : value =
  match b with 
  | Plus -> VInt (l + r)
  | Minus -> VInt (l - r)
  | Times -> VInt (l * r)
  | Equals -> VBool (l = r)
  | Lteq -> VBool (l <= r)
  | Lt -> VBool (l < r)
  | Gteq -> VBool (l >= r)
  | Gt -> VBool (l > r)
  | Neq -> VBool (l <> r)
  | Div -> VInt (l / r)

let string_of_env (env : env) : string =
  "[" 
  ^ (List.fold_left 
       (fun a b -> a ^ "'"^ (b |> expr_of_value |> string_of_exp) ^ "'; ") 
       "" env) 
  ^ "]"

(* For now, lets just do call by value, using big step evaluation *)
let rec eval (exp : lamcom) : lamcom = 
  let final_env = ref [] in
  let rec eval_env (env : env) (exp : lamcom) : value =
    final_env := env;
    (* env |> string_of_env |> print_endline; *)
    match exp with
    (* Eval on left *)
    | App (e1, e2) -> 
      (* print_endline "Evaluating application"; *)
      let v1 = eval_env env e1 in begin 
        match v1 with 
        | Closure (body, env') -> begin 
            let v2 = eval_env env e2 in 
            eval_env (v2::env') body
          end 
        | _ -> failwith "Cannot apply to a non-functon"
      end
    | Var n -> begin 
        (* print_endline "Evaluating variable:"; *)
        match List.nth_opt env n with
        | Some e -> e
        | None -> failwith ("Error: Unbound variable: " ^ (string_of_int n) )
      end
    (* Values step to values *)
    | Int n -> VInt n
    | Bool b -> VBool b
    | Lam e -> 
      (* print_endline "Evaluating lambda";  *)
      Closure (e, env)
    | Unit -> VUnit
    (* The three additional extensions *)
    | If (b, etrue, efalse) -> begin 
        (* print_endline "Evaluating if statement"; *)
        match eval_env env b with 
        | VBool true -> eval_env env etrue
        | VBool false -> eval_env env efalse
        | _ -> failwith "Argument mismatch for if statement"
      end
    | Bop (op, l, r) -> 
      (* print_endline "Evaluating bop"; *)
      let l' = 
        match eval_env env l with 
        | VInt n -> n 
        | _ -> failwith ("Binary operators must take integers on lhs: " ^ string_of_exp (Bop (op, l, r))) 
      in
      let r' = 
        match eval_env env r with 
        | VInt n -> n 
        | _ -> failwith ("Binary operators must take integers on rhs: " ^ string_of_exp (Bop (op, l, r)))
      in 
      eval_bop op l' r'
    | Uop (Neg, e) ->
      begin 
        match eval_env env e with
        | VInt n -> VInt (-n)
        | _ -> failwith ("Neg must take an integer: " ^ string_of_exp (Uop (Neg, e)))
      end 
  in
  let x = eval_env [] exp |> expr_of_value in 
  (* print_endline "Final env:"; *)
  (* !final_env |> string_of_env |> print_endline; *)
  x
