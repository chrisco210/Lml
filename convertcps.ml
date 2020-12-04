open Lambdaast

(* The type of an intermediate ast with features requiring continuations *)
type iast = 
  | App of iast * iast
  | Lam of iast
  | Var of var
  | Int of int
  | Bool of bool
  | Bop of Lambdaast.bop * iast * iast
  | Uop of Lambdaast.uop * iast
  | If of iast * iast * iast
  (* Imperative features that are not converted at this stage *)
  | Seq of iast * iast
  | Ref of iast 
  | While of iast * iast
  | Assign of iast * iast
  | Break
  | Continue

(** [raise_abs_var e tr] raises variables inside of abstractions so they still
    point to the correct lambda level  *)
let rec raise_abs_var (e : iast) (tr : int) : iast =
  match e with 
  | Var v -> if v = tr then Var (1 + 3 * v) else Var v
  | Lam e' -> Lam (raise_abs_var e' (tr + 1)) 
  | App (e1, e2) -> App (raise_abs_var e1 tr, raise_abs_var e2 tr)
  | Int i -> Int i
  | Bool b -> Bool b
  | If (e1, e2, e3) -> If(raise_abs_var e1 tr, raise_abs_var e2 tr, raise_abs_var e3 tr)
  | Bop (b, e1, e2) -> Bop (b, raise_abs_var e1 tr, raise_abs_var e2 tr)
  | Uop (b, e') -> Uop (b, raise_abs_var e' tr)
  | Seq (e1, e2) -> Seq (raise_abs_var e1 tr, raise_abs_var e2 tr)
  | _ -> failwith "Unimplemented raise_abs_var"

(** [convert_cps e] converts an intermediate ast into a lambda expression in 
    continuation passing style.*)
let rec convert_cps (e : iast) : lamcom =
  match e with
  | App (e1, e2) -> 
    Lam (
      App (
        convert_cps e1,
        Lam (
          App (
            convert_cps e2,
            Lam (
              App (
                App (
                  Var 1,
                  Var 0
                ),
                Var 2
              )
            )
          )
        )
      )
    )
  | Lam e' -> Lam (
      App (
        Var 0,
        Lam (
          Lam (
            App (
              convert_cps (raise_abs_var e' 0),
              Var 0
            )
          )
        )
      )
    )
  | Var v -> Lam (App (Var 0, Var (v + 1)))
  | Int i -> Lam (App (Var 0, Int i))
  | Bool b -> Lam (App (Var 0, Bool b))
  | Bop (b, e1, e2) -> failwith "Unimplemented CPS translation"
  | Uop (u, e) -> failwith "Unimplemented CPS translation"
  | If (b, e1, e2) -> failwith "Unimplemented CPS translation"
  (* Imperative features *)
  | Seq (e1, e2) -> failwith "Unimplemented CPS translation"
  | Ref (e) -> failwith "Unimplemented CPS translation"
  | While (e1, e2) -> failwith "Unimplemented CPS translation"
  | Assign (e1, e2) -> failwith "Unimplemented CPS translation"
  | Break -> failwith "Unimplemented CPS translation"
  | Continue -> failwith "Unimplemented CPS translation"