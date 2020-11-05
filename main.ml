open Interp
open Lambdaast

open Pprint

let id_fun = App ((Lam (Var 0)), Int 1)

let _ = id_fun |> eval |> string_of_exp |> print_endline