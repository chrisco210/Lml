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
and  bop = 
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

let rec string_of_ast (e : expr) : string = 
  match e with 
  | Abs (v, e) -> "(L " ^ (v)  ^ " . " ^ (string_of_ast e) ^ ")"
  | App (e1, e2) -> (string_of_ast e1) ^ " " ^ (string_of_ast e2)
  | Var (v) -> v
  | Int (i) -> string_of_int i
  | Bool (b) -> string_of_bool b
  | Bop (e1, bop, e2) -> "(" ^ (string_of_ast e1) ^ (string_of_bop bop) ^ (string_of_ast e2) ^ ")"
  | _ -> failwith "Unimplemented string_of_ast"
and string_of_bop (b : bop) : string = 
  match b with
  | Plus -> "+"
  | Minus -> "-"
  | Equals -> "="
  | Times -> "*"
  | Lteq -> "<="
  | Lt -> "<"
  | Gteq -> ">="
  | Gt -> ">"
  | Neq -> "!="
  | Cons -> "::"
