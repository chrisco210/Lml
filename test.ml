open OUnit2
open Lambdaast
open Interp
open Church

(* Just the identity function *)
let id_fun : lamcom = Lam (Var 0)

(* Y ≜ 𝜆 𝑓 . (𝜆𝑥. 𝑓 (𝑥 𝑥)) (𝜆𝑥. 𝑓 (𝑥 𝑥)). *)
(* 𝐺 ≜ 𝜆 𝑓 . 𝜆𝑛. if 𝑛 = 0 then 1 else 𝑛 × (𝑓 (𝑛 − 1)) *)
(* We use the Y combinator because we implemented call by name semantics *)
let ycomb : lamcom = Lam (
    App (
      (Lam (App (Var 1, App (Var 0, Var 0)))), 
      (Lam (App (Var 1, App (Var 0, Var 0))))
    )
  )

(* The fixed point of this function is the factorial function *)
let fact' : lamcom  = Lam (
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

let fact : lamcom = App (ycomb, fact')

(* The fixed point of this raises a base to an exponent  *)
let pow' : lamcom = 
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

let pow : lamcom = App (ycomb, pow')

(* Nested function *)
let nested : lamcom = App (Lam (Lam (Var 1)), Int 4)
let deepnested : lamcom = Lam (Lam (Lam (Lam (Var 2))))

(* Simple first class function application and evaluation *)
let firstclasssimple : lamcom = App (Lam (Lam (App (Var 1, (App (Var 1, Var 0))))), Lam (Bop (Times, Int 2, Var 0)))


(* Standard boolean encodings  *)
let enc_true : lamcom = Lam (Lam (Var 1))
let enc_false : lamcom = Lam (Lam (Var 0))
let enc_not : lamcom = Lam (App (App (Var 0, enc_false), enc_true))
let enc_and : lamcom = Lam (Lam (App (App (Var 1, Var 0), enc_false)))
let enc_or : lamcom = Lam (Lam (App (App (Var 1, enc_true), Var 0)))

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
  "Pair test" >:: (fun _ ->
      Pair (Int 1, Int 2) |> eval |> assert_equal (Pair (Int 1, Int 2))
    );
  "Evaluating a pair" >:: (fun _ ->
      (App (Lam (Pair (Var 0, Var 0)), Int 1)) |> eval 
      |> assert_equal (Pair (Int 1, Int 1))
    );
  "Evaluating within a pair" >:: (fun _ ->
      Pair (
        (App ((Lam (Var 0)), (Int 2))),
        ((App ((App ((Lam (Lam (Bop (Plus,  Var 0, Var 1)))), (Int 2))), (Int 4))))
      ) |> eval |> assert_equal (Pair (Int 2, Int 6))
    )
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
  make_parse_test "_x'" (Var "_x'");
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
  make_parse_test "let f = fun x -> x + 1 in f 3 + 4" (Let ("f", (Fun (["x"], Bop (Var "x", Plus, Int 1))), Bop (App (Var "f", Int 3), Plus, Int 4)));
  make_parse_test "let rec f = fun a -> f a in 3" (Letrec ("f", ["a"], (App (Var "f", Var "a")), Int 3));
  make_parse_test "let rec f = fun a b c d -> f a in 3" (Letrec ("f", ["a"; "b"; "c"; "d"], (App (Var "f", Var "a")), Int 3));
  make_parse_test "(* comment *) L x . x" (Abs ("x", Var "x"));
  make_parse_test "(* comment *) L x . (*more comments *)x" (Abs ("x", Var "x"));
  make_parse_test "(* cool (*nested (*Comments*)*) *) L x . (*more comments *)x" (Abs ("x", Var "x")); 
  make_parse_test "(a, b)" (Tuple (Var "a", Var "b"));
  make_parse_test "(fun a -> a, fun b -> b)" (Tuple (Fun (["a"], Var "a"), Fun (["b"], Var "b")));
  make_parse_test "let a = (1, 2) in add a" (Let ("a", Tuple (Int 1, Int 2), App (Var "add", Var "a")));
  make_parse_test "a b#0" (App (Var "a", Proj (Var "b", 0)));
  make_parse_test "let x = (1, 2) in x # 0" (Let ("x", (Tuple (Int 1, Int 2)), Proj (Var "x", 0)));
  make_parse_test "fun a -> fun b -> b" (Fun (["a"], (Fun (["b"], Var "b"))));
  make_parse_test "1; 2" (Seq (Int 1, Int 2));
  make_parse_test "unit" Unit;
  make_parse_test "L x . unit" (Abs ("x", Unit));
  make_parse_test "a b; 1" (Seq (App (Var "a", Var "b"), Int 1));
  make_parse_test "fun x -> x; 1" (Fun (["x"], Seq (Var "x", Int 1)));
  make_parse_test "1 + 1; 2; 3" (Seq (Seq (Bop (Int 1, Plus, Int 1), Int 2), Int 3));
  make_parse_test "while true do 1 done" (While (Bool true, Int 1));
  make_parse_test "while false do 1 done; 2" (Seq (While (Bool false, Int 1), Int 2));
  make_parse_test "while false do 1; 2 done; 1" (Seq (While (Bool false, Seq (Int 1, Int 2)), Int 1));
  make_parse_test "while get < 10 do set (get + 1) done; get" 
    (Seq (
        While (
          (Bop (Get, Lt, Int 10)),
          Set (Bop (Get, Plus, Int 1))
        ), Get
      )
    )
]
(* Conversion and execution tests *)
open Convert
let zcombstr = "L f . (L x . f (L y . x x y)) (L x . f (L y . x x y))"

let exec_tests = [
  "Simple application test" >:: (fun _ -> 
      "(L x . x) 1" |> parse |> convert |> eval
      |> assert_equal (Lambdaast.Int 1)
    );
  "Simple Z combinator test" >:: (fun _ ->
      "let Z = " ^ zcombstr ^ " in let G = L f . L n . if n = 0 then 1 else n * (f (n - 1)) in Z G 4"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 24)
    );
  "Let expressions and functions" >:: (fun _ ->
      "let f = fun x -> x + 1 in f (f (f (f (f 1))))" |> parse |> convert |> eval
      |> assert_equal (Lambdaast.Int 6)
    );
  "Functions define as expected" >:: (fun _ -> 
      "let x = 4 in let f = fun z -> z + 4 in let x = 7 in f 6" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 10)
    );
  "Partial application" >:: (fun _ ->
      "let add = fun x y -> x + y in let addtwo = add 2 in addtwo 2"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 4)
    );
  "Recursive definitions work" >:: (fun _ -> 
      "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 4"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 24)
    );
  (* This test is disabled because it takes forever
     "Complex recursive function" >:: (fun _ -> 
      "
      let rec mod = fun a b -> 
        if a < b then a else
          mod (a - b) b
        in
      let rec gcd = fun a b ->
        if a = 0 then b else
        if b = 0 then a else
          let r = mod a b in
          gcd b r 
        in
      gcd 55 10
     " |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 5)
     ); *)
  "Projection works correctly" >:: (fun _ ->
      "(1,2)#0" |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 1)
    );
  "Projection works correctly" >:: (fun _ ->
      "(1,2)#1" |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 2)
    );
  "Projection and application are applied correctly" >:: (fun _ ->
      "(fun x -> x + 1, fun x -> x + 2)#0 1" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 2)
    );
  "Nested tuples are projected correctly" >:: (fun _ -> 
      "(1, (2, 3))#1#1" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 3)
    );
  "More nested tuples" >:: (fun _ ->
      "let a = ((1, 2),(3,4)) in a#0#0 + a#0#1 + a#1#0 + a#1#1"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 10)
    );
  "Curried function test" >:: (fun _ -> 
      "let curry = 
      fun f -> 
      fun tpl -> f tpl#0 tpl#1
      in
      let add = fun a b -> a + b
      in 
      let addcur = curry add in
      addcur (1, 2)
      " |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 3)
    );
  "List operations" >:: (fun _ -> 
      "hd (1::2::3::[])" |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 1)
    );
  "List operations" >:: (fun _ ->
      "hd (tl (1::2::3::[]))" |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 2)
    );
  "List as variable" >:: (fun _ ->
      "let x = 1::2::3::[] in hd x" |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 1)
    );
  "is_nil is correct" >:: (fun _ ->
      "is_nil []" |> parse |> convert |> eval |> assert_equal (Lambdaast.Bool true)
    );
  "is_nil is correct" >:: (fun _ ->
      "is_nil (1::[])" |> parse |> convert |> eval |> assert_equal (Lambdaast.Bool false)
    );
  "Boolean and encodings" >:: (fun _ -> 
      "(if false && false then 1 else 0) 
      + 10 * (if false && true then 1 else 0)
       + 100 * (if true && false then 1 else 0) 
       + 1000 * (if true && true then 1 else 0)"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 1000)
    );
  "Boolean or encodings" >:: (fun _ -> 
      "(if false || false then 1 else 0) 
      + 10 * (if false || true then 1 else 0)
       + 100 * (if true || false then 1 else 0) 
       + 1000 * (if true || true then 1 else 0)"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 1110)
    );
  "Basic reference" >:: (fun _ ->
      "set 1" |> parse |> convert |> eval |> assert_equal (Lambdaast.Unit)
    );
  "Updating refernce as applied to function" >:: (fun _ ->
      "(fun a b c -> get) (set 1) (set 2) (set 3)" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 3)
    );
  "Updating reference before bop" >:: (fun _ -> 
      "set 1; 2 * get" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 2)
    );
  "Update reference in addition" >:: (fun _ ->
      "(set 2; get) * (set 3; get)" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 6)
    );
  "Function reference" >:: (fun _ ->
      "(set fun x -> x); get 4"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 4)
    );
  "Updating reference inside of if" >:: (fun _ -> 
      "if set 1; true then get else 0" |> parse |> convert |> eval 
      |> assert_equal (Lambdaast.Int 1)
    );
  "Recursion with references" >:: (fun _ ->
      "(set (L x . x));
      let fact = fun n -> if n = 0 then 1 else n * (get (n - 1)) in 
      (set (fact)); 
      fact 5" 
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 120)
    );
  "Basic while loop" >:: (fun _ ->
      "while get < 10 do set (get + 1) done; get"
      |> parse |> convert |> eval |> assert_equal (Lambdaast.Int 10)
    )
]

let suite = "LML tests" >::: List.concat [
    lc_interpret_tests; 
    parse_tests; 
    exec_tests
  ]

let _ = run_test_tt_main suite