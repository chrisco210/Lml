(* This conversion will only convert lambda calculus, useful for debuging *)

open Ast
open Lambdaast
open Iast
open Convertcps
open Church

let rec convert_raw_r (e : expr) : iast = 
  match e with
  | Abs(v,e') -> Lam (v, convert_raw_r e')
  | App(e1,e2) -> App (convert_raw_r e1, convert_raw_r e2)
  | Var(v) -> Var v
  | Int(n) -> Int n
  | Bool(b) -> Bool b
  | _ -> failwith "Cannot convert_raw"

let convert (e : expr) : lamcom =
  e |> convert_raw_r |> convert_cps_vars [] 