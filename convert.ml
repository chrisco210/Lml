(* This module does the conversion from an ast to a lambda ast *)

open Ast
open Lambdaast

let convert (e : expr) : lamcom = 
  match e with
  | Let(v,e1,e2) -> failwith "unimplemented in alpha"
  | Letg(v,e') -> failwith "unimplemented in beta"
  | If(e1,e2,e3) -> failwith "unimplemented in alpha"
  | Tuple(e1,e2) -> failwith "unimplemented in beta"
  | Fun(v,e') -> failwith "unimplemented in beta"
  | Proj(e',n) -> failwith "unimplemented in beta"
  | Bop(e1,b,e2) -> failwith "unimplemented in alpha"
  | Uop(u,e') -> failwith "unimplemented in alpha"
  | Seq(e1,e2) -> failwith "unimplemented in gamma"
  | Ref(e') -> failwith "unimplemented in gamma"
  | While(e1,e2) -> failwith "unimplemented in gamma"
  | Assign(e2,e2) -> failwith "unimplemented in gamma"
  | Break -> failwith "unimplemented in gamma"
  | Continue -> failwith "unimplemented in gamma"
  | Abs(v,e') -> failwith "unimplemented in alpha"
  | App(e1,e2) -> failwith "unimplemented in alpha"
  | Hd(l) -> failwith "unimplemented in beta"
  | Tl(l) -> failwith "unimplemented in beta"
  | Nil -> failwith "unimplemented in beta"
  | Var(v) -> failwith "unimplemented in alpha"
  | Int(n) -> failwith "unimplemented in alpha"
  | Bool(b) -> failwith "unimplemented in alpha"