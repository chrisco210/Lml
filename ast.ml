(* This is the module for an ast of Lml *)

type var = string

type expr = 
  (* Regular functional stuff *)
  | Let of var * expr * expr
  | Letg of var * expr
  | If of expr * expr * expr
  | Tuple of expr * expr
  | Fun of (var list) * expr
  | Proj of expr * int

  (* Operations *)
  | Bop of expr * bop * expr
  | Uop of uop * expr

  (* Imperative features *)
  | Seq of expr * expr
  | Ref of expr 
  | While of expr * expr
  | Assign of expr * expr
  | Break
  | Continue

  (* Lambda calculus *)
  | Abs of var * expr
  | App of expr * expr

  (* List operations *)
  | Hd of expr
  | Tl of expr
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