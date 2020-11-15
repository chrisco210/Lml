(* This module does the conversion from an ast to a lambda ast *)

open Ast
open Lambdaast

let list_posn (lst : 'a list) (item : 'a) : int option = 
  let rec list_posni (lst : 'a list) (item : 'a) (n : int) : int option = 
    match lst with
    | h::t when h = item -> Some n
    | h::t -> list_posni t item (n + 1)
    | [] -> None
  in 
  list_posni lst item 0

(** [lambop_of_bop b] is the lambdaast version of a binary operator*)
let lambop_of_bop (b : Ast.bop) : Lambdaast.bop = 
  match b with 
  | Cons -> failwith "Cons cannot be converted directly"
  | Plus -> Plus
  | Minus -> Minus
  | Times -> Times
  | Equals -> Equals
  | Lteq -> Lteq
  | Lt -> Lt
  | Gteq -> Gteq
  | Gt -> Gt
  | Neq -> Neq

(** [convert e] is a lambda calculus translation of an expression e*)
let rec convert (e : expr) : lamcom = 
  let rec convert_var (e : expr) (s : Ast.var list) : lamcom = 
    match e with
    | Abs(v,e') -> Lam (convert_var e' (v::s))
    | App(e1,e2) -> App (convert_var e1 s, convert_var e2 s)
    | Let(v,e1,e2) -> convert_var (App (Abs (v, e2), e1)) s
    | If(e1,e2,e3) -> If (convert_var e1 s, convert_var e2 s, convert_var e3 s)
    | Var(v) -> begin
        match list_posn s v with
        | Some n -> Var n
        | None -> failwith "Unbound variables cannot be converted"
      end
    | Int(n) -> Int n
    | Bool(b) -> Bool b
    | Bop(e1,b,e2) -> begin
        match b with
        | Cons -> failwith "Unimplemented in beta"
        | _ -> Bop (lambop_of_bop b, convert_var e1 s, convert_var e2 s)
      end
    | Fun(v,e') -> List.fold_right (fun elt acc -> Lam acc) v (convert_var e' ((List.rev v) @ s))
    | Uop(u,e') -> failwith "unimplemented in alpha"
    (* Everything below here is unimplemented for alpha *)
    | Letg(v,e') -> failwith "unimplemented in beta"
    | Tuple(e1,e2) -> failwith "unimplemented in beta"
    | Proj(e',n) -> failwith "unimplemented in beta"
    | Seq(e1,e2) -> failwith "unimplemented in gamma"
    | Ref(e') -> failwith "unimplemented in gamma"
    | While(e1,e2) -> failwith "unimplemented in gamma"
    | Assign(e1,e2) -> failwith "unimplemented in gamma"
    | Break -> failwith "unimplemented in gamma"
    | Continue -> failwith "unimplemented in gamma"
    | Hd(l) -> failwith "unimplemented in beta"
    | Tl(l) -> failwith "unimplemented in beta"
    | Nil -> failwith "unimplemented in beta"
  in convert_var e []