open Interp
open Lambdaast
open Pprint




let church (n : int): lamcom = 
  let rec church' (i : int) : lamcom =
    match i with 
    | 0 -> (Var 0)
    | k -> App (Var 1, church' (k - 1))
  in 
  Lam (Lam (church' n))

let church_succ  =
  Lam (Lam (Lam (App (Var 1, App (App (Var 2, Var 1), Var 0)))))

let church_add = 
  Lam (Lam (App (App (Var 1, church_succ), Var 0)))

let church_to_int (n : lamcom) : int =
  match eval (App (App (n, Lam (Bop (Plus, Int 1, Var 0))), Int 0)) with 
  | Int n -> n
  | _ -> failwith "Not church numeral"

let _ = App (App (church_add, church 16), church 32) |>  eval |> church_to_int |> print_int;
  App (App (church_add, church 1), church 2) |>  eval |> string_of_exp |> print_endline;