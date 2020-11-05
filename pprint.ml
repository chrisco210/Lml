open Lambdaast

let rec string_of_exp (e : lamcom) : string = 
  match e with 
  | App (e1, e2) -> (string_of_exp e1) ^ " " ^ (string_of_exp e2)
  | Lam e -> "λ . " ^ (string_of_exp e) 
  | Var n -> "$" ^ (string_of_int n)
  | Int n -> string_of_int n