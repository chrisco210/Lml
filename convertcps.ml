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


let rec raise_abs_var (e : iast) : iast =
  match e with 
  | Var v -> Var (1 + 3 * v)
  | Lam e' -> Lam e' (* Dont raise inside of lambdas *)
  | App (e1, e2) -> App (raise_abs_var e1, raise_abs_var e2)
  | Int i -> Int i
  | Bool b -> Bool b
  | If (e1, e2, e3) -> If(raise_abs_var e1, raise_abs_var e2, raise_abs_var e3)
  | Bop (b, e1, e2) -> Bop (b, raise_abs_var e1, raise_abs_var e2)
  | Uop (b, e') -> Uop (b, raise_abs_var e')
  | Seq (e1, e2) -> Seq (raise_abs_var e1, raise_abs_var e2)
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
              convert_cps (raise_abs_var e'),
              Var 0
            )
          )
        )
      )
    )
  | Var v -> Lam (App (Var 0, Var (v + 1)))
  | Int i -> Lam (App (Var 0, Int i))
  | Bool b -> Lam (App (Var 0, Bool b))
  | Bop (b, e1, e2) -> failwith "Unimplemented"
  | Uop (u, e) -> failwith "Unimplemented"
  | If (b, e1, e2) -> failwith "Unimplemented"
  (* Imperative features *)
  | Seq (e1, e2) -> failwith "Unimplemented"
  | Ref (e) -> failwith "Unimplemented"
  | While (e1, e2) -> failwith "Unimplemented"
  | Assign (e1, e2) -> failwith "Unimplemented"
  | Break -> failwith "Unimplemented"
  | Continue -> failwith "Unimplemented"