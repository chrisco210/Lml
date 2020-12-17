(* This module parses a string *)

open Ast

(** [parse s] is a Lml ast of the string [s]*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let file_to_string (f : string) : string =
  let get_line file = try Some (input_line file) with End_of_file -> None in
  let file = open_in f in
  let rec loop acc =
    match get_line file with
    | Some line when not (0 = String.length line) -> loop (acc ^ line ^ "\n")
    | _ -> acc
  in
  loop ""

let parse_from_file (f : string) : expr =
  let file_as_str = file_to_string f in
  parse file_as_str