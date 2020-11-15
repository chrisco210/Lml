open Ast
open Pprint
open Parse

let () = read_line() |> parse  |> main

let main (e:expr) = let e' = convert e in 
    print_string (string_of_exp e')^" -> "^(strign_of_exp (eval e'))