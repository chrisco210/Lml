(** This module interprets a file*)
open Parse
open Ast
open Interp
open Convert
open Pprint
open Lambdaast

let eval_and_print (e:expr) : unit = let e' = convert e in 
  print_endline ("-> " ^ (string_of_exp e'));
  print_endline ("-> " ^ (e' |> eval |> string_of_exp))

let interp_file f = f |> parse_from_file |> eval_and_print