(* This conversion will only convert lambda calculus, useful for debuging *)

open Ast
open Lambdaast
open Iast
open Convertcps
open Church

(** [lambop_of_bop b] is the lambdaast version of a binary operator*)
let lambop_of_bop (b : Ast.bop) : Lambdaast.bop = 
  match b with 
  | Cons | And | Or -> failwith "Cannot be converted directly"
  | Plus -> Plus
  | Minus -> Minus
  | Times -> Times
  | Equals -> Equals
  | Lteq -> Lteq
  | Lt -> Lt
  | Gteq -> Gteq
  | Gt -> Gt
  | Neq -> Neq
  | Div -> Div

let rec convert_raw_r (e : expr) : iast = 
  match e with
  | Abs(v,e') -> Lam (v, convert_raw_r e')
  | App(e1,e2) -> App (convert_raw_r e1, convert_raw_r e2)
  | Var(v) -> Var v
  | Int(n) -> Int n
  | Bool(b) -> Bool b
  | If (e1, e2, e3) -> If (convert_raw_r e1, convert_raw_r e2, convert_raw_r e3)
  | Bop(e1,b,e2) -> Bop (lambop_of_bop b, convert_raw_r e1, convert_raw_r e2)
  | _ -> failwith "Cannot convert_raw"

let convert (e : expr) : lamcom =
  e |> convert_raw_r |> convert_cps_vars [] 