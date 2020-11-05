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

type var = string

type lamcom = 
  | App of lamcom * lamcom
  | Abs of var * lamcom
  | Var of var
  | Int of int