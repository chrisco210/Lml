(* The AST of De Bruijn lambda calculus extended with integers operators and 
   if statements *)

type bop = 
  | Plus 
  | Minus
  | Times
  | Div
  | Equals
  | Lteq
  | Lt
  | Gteq
  | Gt
  | Neq
type uop =
  | Not
  | Neg

type var = int

type lamcom = 
  | App of lamcom * lamcom
  | Lam of lamcom
  | Var of var
  | Int of int
  | Bool of bool
  | Unit
  | Bop of bop * lamcom * lamcom
  | Uop of uop * lamcom
  | If of lamcom * lamcom * lamcom
  | Pair of lamcom * lamcom
  | Proj of int * lamcom

