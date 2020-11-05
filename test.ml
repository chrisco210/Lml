open OUnit2
open Lambdaast
open Interp

let id_fun = Lam (Var 0)

(* 𝜆 𝑓 . (𝜆𝑥. 𝑓 (𝜆𝑦. 𝑥 𝑥 𝑦)) (𝜆𝑥. 𝑓 (𝜆𝑦. 𝑥 𝑥 𝑦)) *)
(* (𝜆𝑥. 𝑓 (𝜆𝑦. 𝑥 𝑥 𝑦) *)
let partial = 
  Lam (
    App (
      Var 1,
      (Lam (
          App(App(Var 1, Var 1), (Var 0))
        ))
    )
  )
let z_com = Lam (App (partial, partial))

(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)
let fact' = Lam (Lam (If ((Bop (Equals, (Var 0), (Int 0))), (Int 1), (Bop (Times, Var 0, App (Var 1, (Bop (Minus, Var 1, Int 1))))))))

let fact = App (z_com, fact')

let nested = App (Lam (Lam (Var 1)), Int 4)

let deepnested = Lam (Lam (Lam (Lam (Var 2))))

let firstclasssimple = App (Lam (Lam (App (Var 1, (App (Var 1, Var 0))))), Lam (Bop (Times, Int 2, Var 0)))

let lc_interpret_tests = [
  "Identity function is value" >:: (fun _ ->
      id_fun |> eval |> assert_equal (id_fun)
    );
  "Identity function applied" >:: (fun _ -> 
      (App (id_fun, Int 5)) |> eval |> assert_equal (Int 5)
    );
  "Identity function applied to variable" >:: (fun _ ->
      (App (id_fun, Var 0)) |> eval |> assert_equal (Var 0)
    );
  "Nested lambdas applied give function that returns 4" >:: (fun _ -> 
      (nested |> eval |> assert_equal (Lam (Int 4)))
    );
  "Applying to an application returns 4" >:: (fun _ -> 
      (App (nested, Int 999) |> eval |> assert_equal (Int 4))
    );
  "Applying to deep nested" >:: (fun _ -> 
      App (App (App (App (deepnested, Int 3), Int 2), Int 1), Int 0)
      |> eval |> assert_equal (Int 2)
    );
  "First class function test" >:: (fun _ -> 
      App (firstclasssimple, Int 2) |> eval |> assert_equal (Int 8)
    )
]


let suite = "LML tests" >::: List.concat [lc_interpret_tests]

let _ = run_test_tt_main suite