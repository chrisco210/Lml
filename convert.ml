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

let lambop_of_uop (u : Ast.uop) : Lambdaast.uop =
  match u with
  | Not -> Not
  | Neg -> Neg
  | Deref -> failwith "Deref cannot be converted directly"

(** [convert e] is a lambda calculus translation of an expression e*)
let rec convert (e : expr) : lamcom = 
  let rec convert_var (e : expr) (s : Ast.var list) : lamcom = 
    match e with
    | Abs(v,e') -> Lam (convert_var e' (v::s))
    | App(e1,e2) -> App (convert_var e1 s, convert_var e2 s)
    | Let(v,e1,e2) -> convert_var (App (Abs (v, e2), e1)) s
    (* Uses the Y combinator for recursion *)
    | Letrec (v, va, e, b) -> let ycomb = Lam (
        App (
          (Lam (App (Var 1, App (Var 0, Var 0)))), 
          (Lam (App (Var 1, App (Var 0, Var 0))))
        )
      ) in 
      let converted = convert_var (Abs (v, Fun (va, e))) s in 
      let convbody = convert_var (Abs (v, b)) s in
      App (convbody, App (ycomb, converted))
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
        | And -> If (convert_var e1 s, convert_var e2 s, Bool false)
        | Or -> If (convert_var e1 s, Bool true, convert_var e2 s)
        | _ -> Bop (lambop_of_bop b, convert_var e1 s, convert_var e2 s)
      end
    | Fun(v,e') -> List.fold_right 
                     (fun elt acc -> Lam acc) 
                     v 
                     (convert_var e' ((List.rev v) @ s))
    | Uop(u,e') -> begin
        match u with
        | Deref -> failwith "Unimplemented in gamma"
        | _ -> Uop (lambop_of_uop u, convert_var e' s)
      end
    (* Everything below here is unimplemented for alpha *)
    | Letg(v,e') -> failwith "unimplemented in beta"
    (* Using the encoding from 
       https://en.wikipedia.org/wiki/Church_encoding#Church_pairs *)
    | Tuple(e1,e2) -> 
      App (App (Church.pair,  (convert_var e1 s)), convert_var e2 s)
    | Proj(e',n) -> let rec proj_rec (en : lamcom) (n : int) =  
                      match n with
                      | 0 -> (Lambdaast.App (Church.fst, en))
                      | 1 -> (Lambdaast.App (Church.snd, en))
                      | _ -> failwith "n-ary tuples are not implemented"
      in proj_rec (convert_var e' s) n
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