type ivar = string

(* The type of an intermediate ast with features requiring continuations *)
type iast = 
  | App of iast * iast
  | Lam of ivar * iast
  | Var of ivar
  | Int of int
  | Unit
  | Bool of bool
  | Bop of Lambdaast.bop * iast * iast
  | Uop of Lambdaast.uop * iast
  | If of iast * iast * iast
  (* Imperative features that are not converted at this stage *)
  | Seq of iast * iast
  | Ref of iast 
  | Deref of iast
  | While of iast * iast
  | Assign of iast * iast
  | Break
  | Continue

  | Get 
  | Set of iast
