(* This module does the conversion from an ast to an intermediate lambda  *)

open Ast
open Lambdaast
open Iast
open Convertcps
open Church

(* L x . L y . L z . z x y *)
let pair = Lam ("***x", Lam ("***y" ,Lam ("***z", App (App (Var "***z", Var "***x"), Var "***y"))))

(* L p . p (L x . L y . x) *)
let fst = Lam ("***p", App (Var "***p", Lam ("***x", Lam ("***y", Var "***x"))))

(* L p . p (L x . L y . y) *)
let snd = Lam ("***p", App (Var "***p", Lam ("***x", Lam ("***y", Var "***y"))))



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
  | Hd -> failwith "Hd cannot be converted directly"
  | Tl -> failwith "Tl cannot be converted directly"

(** [convert e] is a lambda calculus translation of an expression e*)
let rec convert_var (e : expr) : iast = 
  match e with
  | Abs(v,e') -> Lam (v, convert_var e')
  | App(e1,e2) -> App (convert_var e1, convert_var e2)
  | Let(v,e1,e2) -> (App (Lam (v, convert_var e2), convert_var e1)) 
  (* Uses the Z combinator for recursion *)
  | Letrec (v, va, e, b) -> 
    let zcomb = Lam ("**f", 
                     App (
                       (Lam ("**x", App (Var "**f", Lam ("**y", App (App (Var "**x", Var "**x"), Var "**y"))))), 
                       (Lam ("**x", App (Var "**f", Lam ("**y", App (App (Var "**x", Var "**x"), Var "**y"))))) 
                     )
                    ) in 
    let converted = convert_var (Abs (v, Fun (va, e)))  in 
    let convbody = convert_var (Abs (v, b))  in
    App (convbody, App (zcomb, converted))
  | If(e1,e2,e3) -> If (convert_var e1, convert_var e2, convert_var e3)
  | Var(v) -> Var v
  | Int(n) -> Int n
  | Bool(b) -> Bool b
  | Bop(e1,b,e2) -> begin
      match b with
      | Cons -> App (App (pair,  Bool false), App (App (pair, convert_var e1), convert_var e2))
      | And -> If (convert_var e1, convert_var e2, Bool false)
      | Or -> If (convert_var e1, Bool true, convert_var e2)
      | _ -> Bop (lambop_of_bop b, convert_var e1, convert_var e2)
    end
  | Fun(vs,e') -> 
    let converted_body = convert_var e' in
    List.fold_right (fun v acc -> Lam (v, acc)) vs converted_body
  | Uop(u,e') -> begin
      match u with
      | Hd -> App (fst, App (snd, convert_var e'))
      | Tl -> App (snd, App (snd, convert_var e'))
      | _ -> Uop (lambop_of_uop u, convert_var e')
    end
  | Letg(v,e') -> failwith "unimplemented"
  (* Using the encoding from 
     https://en.wikipedia.org/wiki/Church_encoding#Church_pairs *)
  | Tuple(e1,e2) -> 
    App (App (pair,  (convert_var e1)), convert_var e2)
  | Proj(e',n) -> let rec proj_rec (en : iast) (n : int) : iast =  
                    match n with
                    | 0 -> (App (fst, en))
                    | 1 -> (App (snd, en))
                    | _ -> failwith "n-ary tuples are not implemented"
    in proj_rec (convert_var e') n
  | Seq(e1,e2) -> Seq(convert_var e1, convert_var e2)
  | Ref(e') -> Ref (convert_var e')
  | Deref (e') -> Deref (convert_var e')
  | While(e1,e2) -> While (convert_var e1, convert_var e2)
  | Assign(e1,e2) -> Assign (convert_var e1, convert_var e2)
  | Break -> Break
  | Continue -> Continue
  | Nil -> App (App (pair,  Bool true), Bool true)
  | IsNil(e) -> App (fst, convert_var e)
  | Unit -> Unit

let rec convert (e : expr) : lamcom = 
  let translated = convert_var e |> convert_cps in 
  App (translated, Lam (Var 0))
