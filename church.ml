(* A file to contain church encodings for use in the project *)

open Lambdaast

(* L x . L y . L z . z x y *)
let pair = Lam (Lam (Lam (App (App (Var 0, Var 2), Var 1))))

(* L p . p (L x . L y . x) *)
let fst = Lam (App (Var 0, Lam (Lam (Var 1))))

(* L p . p (L x . L y . y) *)
let snd = Lam (App (Var 0, Lam (Lam (Var 0))))