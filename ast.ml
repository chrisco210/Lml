(* This is the module for an ast of Lml *)

type var = string

type com = 
  (* Regular functional stuff *)
  | Let of var * com * com
  | Letg of var * com
  | If of com * com * com
  | Tuple of com * com
  | Fun of (var list) * com
  | Proj of com * int

  (* Operations *)
  | Bop of com * bop * com
  | Uop of uop * com

  (* Imperative features *)
  | Seq of com * com
  | Ref of com 
  | While of com * com
  | Assign of com * com
  | Break
  | Continue

  (* Lambda calculus *)
  | Abs of var * com
  | App of com * com

  (* List operations *)
  | Hd of com
  | Tl of com
  | Nil

  (* Literals *)
  | Var of var
  | Int of int
  | Bool of bool
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
  | Cons
and uop =
  | Not
  | Neg 
  | Deref