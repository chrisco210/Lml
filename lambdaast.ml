(* The AST of De Bruijn lambda calculus extended with integers *)

type bop = 
  | Plus 
  | Minus
  | Times
  | Equals
  | Lteq
  | Lt
  | Gteq
  | Gt
  | Neq

type var = int

type lamcom = 
  | App of lamcom * lamcom
  | Lam of lamcom
  | Var of var
  | Int of int