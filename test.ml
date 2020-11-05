open OUnit2
open Lambdaast
open Interp

let id_fun = Lam (Var 0)

(* Y ≜ 𝜆 𝑓 . (𝜆𝑥. 𝑓 (𝑥 𝑥)) (𝜆𝑥. 𝑓 (𝑥 𝑥)). *)
(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)

let ycomb = Lam (
    App (
      (Lam (App (Var 1, App (Var 0, Var 0)))), 
      (Lam (App (Var 1, App (Var 0, Var 0))))
    )
  )
let fact' = Lam (
    Lam (
      If (
        (Bop (Equals, (Var 0), (Int 0))),
        (Int 1),
        (Bop 
           (
             Times, 
             (Var 0), 
             (
               App (
                 (Var 1), 
                 (Bop (Minus, (Var 0), (Int 1)))
               )
             )
           )
        )
      )
    )
  )

let fact = App (ycomb, fact')

(* lambda f : Int -> Int -> Int. lambda b : Int . lambda e : Int .  if e = 0 then 1 else b * (f b (e - 1)) *)

let pow' = 
  Lam (
    Lam (
      Lam (
        If (
          (Bop (Equals, (Var 0), (Int 0))),
          (Int 1),
          (Bop (
              Times,
              (Var 1),
              (App (
                  (App (
                      (Var 2),
                      (Var 1)
                    ),
                   (Bop (
                       Minus,
                       (Var 0),
                       (Int 1)
                     )
                   )

                  )
                )
              )
            )
          )
        )
      )
    )
  )

let pow = App (ycomb, pow')

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
    );
  "Application involving binary operators" >:: (fun _ -> 
      App (App (App (id_fun, Lam (Lam (Bop (Plus, Var 1, Var 0)))), Int 4), Int 4) 
      |> eval 
      |> assert_equal (Int 8)
    );
  "Application of y combinator based function" >:: (fun _ -> 
      App (fact, (Int 4)) |> eval |> assert_equal (Int 24)
    );
  "Application of y combinator based function" >:: (fun _ -> 
      App (fact, (Int 0)) |> eval |> assert_equal (Int 1)
    );
  "Application of y combinator based function" >:: (fun _ -> 
      App (fact, (Int 8)) |> eval |> assert_equal (Int 40320)
    );
  "Two variable y combinator function" >:: (fun _ ->
      App (App (pow, Int 10), Int 3) |> eval |> assert_equal (Int 1000)
    )
]


let suite = "LML tests" >::: List.concat [lc_interpret_tests]

let _ = run_test_tt_main suite