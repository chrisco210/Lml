open Ast
open Pprint
open Parse
open Convert
open Pprint
open Interp


let eval_and_print (e:expr) : unit = let e' = convert e in 
  print_endline ("Converts to: " ^ (string_of_exp e'));
  print_endline ("Evaluates to: " ^ (e' |> eval |> string_of_exp))

let rec main () = 
  print_endline "--------------------------------------------------------------------------------";
  print_string ">";
  (* Exception handling based on https://ocaml.org/learn/tutorials/error_handling.html  *)
  let input = read_line () in
  begin
    try input |> parse |> eval_and_print with
      e -> let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
      print_endline ("Error evaluating or parsing: " ^ msg ^ stack)
  end;
  main ()

let repl () = 
  print_endline "--------------------------------------------------------------------------------";
  print_endline "                                    LML REPL                                    ";
  print_endline "--------------------------------------------------------------------------------";
  print_endline "Press ^C to quit, see README.md for syntax help and some examples to run";
  main ()

let args (pargs : string list) : string option = 
  match pargs with 
  | _::infile::[] -> Some infile
  | _ -> None

let _ = match args (Array.to_list Sys.argv) with
  | Some path ->  path |> parse_from_file |> eval_and_print
  | None -> repl ()