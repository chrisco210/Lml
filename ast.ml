(* This is the module for an ast of Lml *)

type var = string

type com = 
  | Let of com * var * com
  | Tuple of com * com
  | Bop of com * bop * com
  | Uop of uop * com
  | Fun of var * com
  | Proj of com * int
  | Hd of com
  | Tl of com
  | Var of var
  | Int of int
and bop = 
  | Plus 
  | Minus
  | Times
  | Equals
  | Lteq
  | Lt
  | Gteq
  | Gt
  | Neq
and uop =
  | Not of com
  | Neg of com