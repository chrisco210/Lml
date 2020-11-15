open OUnit2
open Lambdaast
open Interp

(* Just the identity function *)
let id_fun = Lam (Var 0)

(* Y ≜ 𝜆 𝑓 . (𝜆𝑥. 𝑓 (𝑥 𝑥)) (𝜆𝑥. 𝑓 (𝑥 𝑥)). *)
(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)
(* We use the Y combinator because we implemented call by name semantics *)
let ycomb = Lam (
    App (
      (Lam (App (Var 1, App (Var 0, Var 0)))), 
      (Lam (App (Var 1, App (Var 0, Var 0))))
    )
  )

(* The fixed point of this function is the factorial function *)
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

(* The fixed point of this raises a base to an exponent  *)
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

(* Nested function *)
let nested = App (Lam (Lam (Var 1)), Int 4)
let deepnested = Lam (Lam (Lam (Lam (Var 2))))

(* Simple first class function application and evaluation *)
let firstclasssimple = App (Lam (Lam (App (Var 1, (App (Var 1, Var 0))))), Lam (Bop (Times, Int 2, Var 0)))


(* Standard boolean encodings  *)
let enc_true = Lam (Lam (Var 1))
let enc_false = Lam (Lam (Var 0))
let enc_not = Lam (App (App (Var 0, enc_false), enc_true))
let enc_and = Lam (Lam (App (App (Var 1, Var 0), enc_false)))
let enc_or = Lam (Lam (App (App (Var 1, enc_true), Var 0)))

(* Generates a church numeral for n *)
let church (n : int): lamcom = 
  let rec church' (i : int) : lamcom =
    match i with 
    | 0 -> (Var 0)
    | k -> App (Var 1, church' (k - 1))
  in 
  Lam (Lam (church' n))

(* Imnplements the standard church successor function *)
let church_succ  =
  Lam (Lam (Lam (App (Var 1, App (App (Var 2, Var 1), Var 0)))))

(* Implements standard church addition *)
let church_add = 
  Lam (Lam (App (App (Var 1, church_succ), Var 0)))

(* Convert a church numeral to an integer to make testing equivalence easier *)
let church_to_int (n : lamcom) : int =
  match eval (App (App (n, Lam (Bop (Plus, Int 1, Var 0))), Int 0)) with 
  | Int n -> n
  | _ -> failwith "Not church numeral"

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
    );
  "Encoded booleans application example" >:: (fun _ -> 
      App (App (enc_or, enc_true), enc_false) |> eval |> assert_equal (enc_true)
    );
  "Encoded booleans application example" >:: (fun _ -> 
      App (App (enc_or, enc_true), enc_true) |> eval |> assert_equal (enc_true)
    );
  "Encoded booleans application example" >:: (fun _ -> 
      App (App (enc_and, App (App (enc_or, enc_true), enc_true)), enc_false) |> eval |> assert_equal (enc_false)
    );
  "Encoded booleans application example" >:: (fun _ -> 
      App (enc_not, App (App (enc_and, App (App (enc_or, enc_true), enc_true)), enc_false)) |> eval |> assert_equal (enc_true)
    );
  "Church addition" >:: (fun _ -> 
      App (App (church_add, church 3), church 2) |> eval |> church_to_int |> assert_equal 5
    );
  "Church addition" >:: (fun _ -> 
      App (App (church_add, church 3), church 0) |> eval |> church_to_int |> assert_equal 3
    );
  "Church addition" >:: (fun _ -> 
      App (App (church_add, church 55), church 33) |> eval |> church_to_int |> assert_equal (55 + 33)
    );
]


(* ---------------------- Parser tests --------------------------- *)

open Parse
open Ast

let make_parse_test (s : string) (e : expr) =
  s ^ " parses to " ^ (string_of_ast e) >:: (fun _ -> 
      s |> parse |> assert_equal e
        ~printer:string_of_ast)

(* Sorry for the long line lengths, I think it looks better this way *)
(* These programs may or may not be valid, they are just to test the parser *)
let parse_tests = [
  make_parse_test "f a b c" (App (App (App (Var "f", Var "a"), Var "b"), Var "c"));
  make_parse_test "f a (b c)" (App (App (Var "f", Var "a"), App (Var "b", Var "c")));
  make_parse_test "(L x . x) (L y . y) (L z . z)" (App (App ((Abs ("x", Var "x")), (Abs ("y", Var "y"))), (Abs ("z", Var "z"))));
  make_parse_test "Lx" (Ast.Var "Lx");
  make_parse_test "L x.x" (Ast.Abs ("x", Ast.Var "x"));
  make_parse_test "L    x    .    x" (Ast.Abs ("x", Ast.Var "x"));
  make_parse_test "L x . x + 1" (Ast.Abs ("x", (Ast.Bop (Ast.Var "x", Ast.Plus, Ast.Int 1))));
  make_parse_test "L x . 1 + 2 + x" (Ast.Abs ("x", (Ast.Bop (Ast.Bop (Ast.Int 1, Ast.Plus, Ast.Int 2), Ast.Plus, Ast.Var "x"))));
  make_parse_test "L x . 1 + (2 + x)" (Ast.Abs ("x", (Ast.Bop (Ast.Int 1, Ast.Plus, Ast.Bop (Ast.Int 2, Ast.Plus, Ast.Var "x")))));
  make_parse_test "L x . L y . x" (Ast.Abs ("x", Ast.Abs ("y", Ast.Var "x")));
  make_parse_test "(L x . x) L x . x" (Ast.App ((Ast.Abs ("x", Ast.Var "x")), (Ast.Abs ("x", Ast.Var "x"))));
  make_parse_test "L x . (x L x . x)" (Ast.Abs ("x", Ast.App (Ast.Var "x", Ast.Abs ("x", Var "x"))));
  make_parse_test "L x . x L x . x" (Ast.Abs ("x", Ast.App (Ast.Var "x", Ast.Abs ("x", Var "x"))));
  make_parse_test "L x . x x" (Ast.Abs ("x", Ast.App (Ast.Var "x", Ast.Var "x")));
  make_parse_test "let x = 1 in x" (Ast.Let ("x", Ast.Int 1, Ast.Var "x"));
  make_parse_test "let cool = L x . x in cool 1" (Ast.Let ("cool", (Ast.Abs ("x", Ast.Var "x")), (Ast.App (Ast.Var "cool", Ast.Int 1))));
  make_parse_test "let y = (L x . x) 5 in y + 3" (Ast.Let ("y", (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Int 5)), Ast.Bop (Ast.Var "y", Ast.Plus, Ast.Int 3)));
  make_parse_test "let x = let y = 5 in y in x" (Ast.Let ("x", (Ast.Let ("y", Ast.Int 5, Ast.Var "y")), Ast.Var "x"));
  make_parse_test "let x = let y = (L x . x) 5 in y in x" (Ast.Let ("x", (Ast.Let ("y", (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Int 5)), Ast.Var "y")), Ast.Var "x"));
  make_parse_test "let x = let y = (L x . x) 5 in y y in x" (Ast.Let ("x", (Ast.Let ("y", (Ast.App (Ast.Abs ("x", Ast.Var "x"), Ast.Int 5)), Ast.App (Ast.Var "y", Ast.Var "y"))), Ast.Var "x"));
  make_parse_test "if a then b else c" (Ast.If (Ast.Var "a", Ast.Var "b", Ast.Var "c"));
  make_parse_test "(L x . x) 5 < 3" (Bop (App (Abs ("x", Var "x"), Int 5), Lt, Int 3));
  make_parse_test "f 5 < 3" (Bop (App (Var "f", Int 5), Lt, Int 3));
  make_parse_test "if (L x . x) 5 < 3 then L x . x else L y . y" (If ((Bop (App (Abs ("x", Var "x"), Int 5), Lt, Int 3)), (Abs ("x", Var "x")), (Abs ("y", Var "y"))));
  make_parse_test "fun x -> x" (Fun (["x"], Var "x"));
  make_parse_test "fun a b c d e f g -> 1" (Fun (["a"; "b"; "c"; "d"; "e"; "f"; "g"], Int 1));
  make_parse_test "fun x y -> x y" (Fun (["x"; "y"], (App (Var "x", Var "y"))));
  make_parse_test "let f = fun x -> x + 1 in f 3 + 4" (Let ("f", (Fun (["x"], Bop (Var "x", Plus, Int 1))), Bop (App (Var "f", Int 3), Plus, Int 4)))
]

(* ------------------------ Conversion tests ----------------------------------- *)

open Convert

let convert_tests = [
  "Converting ID function" >:: (fun _ -> 
      (Ast.Abs ("x", Ast.Var "x")) |> convert |> assert_equal (Lam (Var 0))
    );
  "Nested lambdas" >:: (fun _ -> 
      (Ast.Abs ("x", Ast.Abs ("y", Ast.App (Ast.Var "x", Ast.Var "y")))) 
      |> convert |> assert_equal (Lam (Lam (App (Var 1, Var 0))))
    );
  "Lambda with binop" >:: (fun _ ->
      (Ast.Abs ("x", (Ast.Bop (Ast.Var "x", Ast.Plus, Ast.Int 1))))
      |> convert |> assert_equal (Lam (Bop (Plus, Var 0, Int 1)))
    );
  "Lambda with nested binops" >:: (fun _ ->
      (Ast.Abs ("x", (Ast.Bop (Ast.Bop (Ast.Int 1, Ast.Plus, Ast.Int 2), Ast.Plus, Ast.Var "x"))))
      |> convert |> assert_equal (Lam (Bop (Plus, Bop (Plus, Int 1, Int 2), Var 0)))
    );
  (* We already tested parsing above, so for convenience use parsing in the following tests *)
  "Lambda with variable redefinition" >:: (fun _ ->
      "L x . L y . L x . x" |> parse |> convert
      |> assert_equal (Lam (Lam (Lam (Var 0))))
    );
  "Lambda with two different variables with same name at different depths" >::
  (fun _ ->
     "L x. x L y . L x . x y" |> parse |> convert
     |> assert_equal (Lam (App (Var 0, Lam (Lam (App (Var 0, Var 1))))))
  );
  "Simple let expression" >:: (fun _ ->
      "let x = 4 in x" |> parse |> convert
      |> assert_equal (Lambdaast.App (Lam (Var 0), Int 4))
    );
  "Omega conversion" >:: (fun _ ->
      let partial = Lam (App (Var 0, Var 0)) in
      "(L x . x x) (L x . x x)" |> parse |> convert
      |> assert_equal (Lambdaast.App (partial, partial))
    );
]

(* Conversion and execution tests *)
(* These are more simple because we test the interpreter parsing and conversion 
   seperately above. *)

(* Y ≜ 𝜆 𝑓 . (𝜆𝑥. 𝑓 (𝑥 𝑥)) (𝜆𝑥. 𝑓 (𝑥 𝑥)). *)
(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)

let ycombstr = "L f . (L x . f (x x)) (L x . f (x x))"

let exec_tests = [
  "Simple application test" >:: (fun _ -> 
      "(L x . x) 1" |> parse |> convert |> eval
      |> assert_equal (Lambdaast.Int 1)
    );
  "Simple Y combinator test" >:: (fun _ ->
      "let Y = " ^ ycombstr ^ " in let G = L f . L n . if n = 0 then 1 else n * (f (n - 1)) in Y G 4"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 24)
    );
]

let suite = "LML tests" >::: List.concat [lc_interpret_tests; parse_tests; convert_tests; exec_tests]

let _ = run_test_tt_main suite