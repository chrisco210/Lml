open Ast
open Pprint

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let _ = "L x . x" |> parse  |> string_of_ast |> print_endline