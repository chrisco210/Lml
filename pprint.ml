(** This module is a pretty printer for the lambda ast*)

open Lambdaast

let string_of_bop (b : bop) : string = match b with
  | Plus -> "+"
  | Minus -> "-"
  | Equals -> "="
  | Times -> "*"
  | Lteq -> "<="
  | Lt -> "<"
  | Gteq -> ">="
  | Gt -> ">"
  | Neq -> "!="
  | Div -> "/"

let string_of_uop (u : uop) : string = match u with
  | Neg -> "~-"

(** [string_of_exp e] is a string representing the lambda calculus expression e
*)
let rec string_of_exp (e : lamcom) : string = 
  match e with 
  | App (e1, e2) -> "(" ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ")"
  | Lam e -> "(L . " ^ (string_of_exp e) ^ ")"
  | Var n -> "$" ^ (string_of_int n)
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | If (b, etrue, efalse) -> "IF " ^ (string_of_exp b) ^ " THEN " ^ (string_of_exp etrue) ^ " ELSE " ^ (string_of_exp efalse)
  | Bop (bop, l, r) -> (string_of_exp l) ^ (string_of_bop bop) ^ (string_of_exp r)
  | Uop (uop, e) -> (string_of_uop uop) ^ (string_of_exp e)
  | Unit -> "unit"

