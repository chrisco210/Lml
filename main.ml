open Interp
open Lambdaast
open Pprint

let id_fun = App ((Lam (Var 0)), Int 1)


let partial = 
  Lam (
    App (
      Var 1,
      (Lam (
          App(Var 1, (App (Var 1, (Var 0))))
        ))
    )
  )

let z_com = Lam (App (partial, partial))

(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)
let fact' = Lam (Lam (If ((Bop (Equals, (Var 0), (Int 0))), (Int 1), (Bop (Times, (Var 0), (App (Var 1, (Bop (Minus, (Var 1), (Int 1))))))))))

let fact = App (z_com, fact')

let _ = App (fact, Int 4) |>  eval |> string_of_exp |> print_endline