open Ast
open Pprint
open Parse

let _ = "L x . 1 + 2 + x" |> parse  |> string_of_ast |> print_endline