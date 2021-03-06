(** [ast] is a module with types representing the AST of LML*)

type var = string

(** [expr] an LML expression*)
type expr = 
  (* Regular functional stuff *)
  | Let of var * expr * expr
  | Letrec of var * (var list) * expr * expr
  | Letg of var * expr
  | If of expr * expr * expr
  | Tuple of expr list
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
  | Deref of expr
  | Break
  | Continue

  | Get 
  | Set of expr

  (* Lambda calculus *)
  | Abs of var * expr
  | App of expr * expr

  | Nil
  | IsNil of expr

  (* Literals *)
  | Var of var
  | Int of int
  | Bool of bool
  | Unit
  (** Binary operators*)
and  bop = 
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
  | And
  | Or
  | Cons
  (** Unary Operators*)
and uop =
  | Not
  | Neg 
  | Hd 
  | Tl 

(** [string_of_ast e] is a string representing the expression e*)
let rec string_of_ast (e : expr) : string = 
  match e with 
  | Abs (v, e) -> "(L " ^ (v)  ^ " . " ^ (string_of_ast e) ^ ")"
  | App (e1, e2) -> "(" ^ (string_of_ast e1) ^ " " ^ (string_of_ast e2) ^ ")"
  | Var (v) ->  v
  | Int (i) -> string_of_int i
  | Bool (b) -> string_of_bool b
  | Bop (e1, bop, e2) -> "(" ^ (string_of_ast e1) ^ (string_of_bop bop) ^ (string_of_ast e2) ^ ")"
  | Letrec (v, va, e, b) -> "let " ^ v ^ " = fun " ^ (List.fold_left (fun a b ->  a ^ " " ^ b) "" va) ^ "->" ^ (string_of_ast e) ^ " in " ^ (string_of_ast b)
  | Let (v, e1, e2) -> "(let " ^ v ^ " = " ^ (string_of_ast e1) ^ " in " ^ (string_of_ast e2) ^ ")"
  | If (e1, e2, e3) -> "if (" ^ (string_of_ast e1) ^ ") then (" ^ (string_of_ast e2) ^ ") else (" ^ (string_of_ast e3) ^ ")"
  | Fun (v, e) -> "fun" ^ (List.fold_left (fun a b ->  a ^ " " ^ b) "" v) ^ " -> (" ^ (string_of_ast e) ^ ")"
  | Tuple (es) -> "(" ^ (List.fold_left (fun a b ->  a ^ "," ^ (string_of_ast b)) "" es) ^ ")"
  | Proj (e, n) -> (string_of_ast e) ^ "#" ^ (string_of_int n)
  | Seq (e1, e2) -> (string_of_ast e1) ^ "; " ^ (string_of_ast e2)
  | Uop (uop, e) -> (string_of_uop uop) ^ (string_of_ast e)
  | Ref (e) -> "ref " ^ (string_of_ast e)
  | Deref e -> "!" ^ (string_of_ast e)
  | While (e1, e2) -> "while (" ^ (string_of_ast e1) ^ ") do (" ^ (string_of_ast e2) ^ ")"
  | Assign (e1, e2) -> (string_of_ast e1) ^ " := " ^ (string_of_ast e2) 
  | Break -> "break"
  | Continue -> "continue"
  | Nil -> "[]"
  | IsNil(e) -> "is_nil" ^ (string_of_ast e)
  | Letg (v, e) -> "let " ^ v ^ " = " ^ (string_of_ast e)
  | Unit -> "unit"
  | Get -> "get"
  | Set e -> "set" ^ (string_of_ast e)
and string_of_uop (o : uop) : string =
  match o with
  | Neg -> "~-"
  | Not -> "~"
  | Hd -> "hd"
  | Tl -> "tl"
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
  | And -> "&&"
  | Or -> "||"
  | Div -> "/"
