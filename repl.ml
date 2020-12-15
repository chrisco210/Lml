module type Converter = sig 
  open Ast
  open Lambdaast

  (** [convert e] is a lambda calculus translation of an expression e*)
  val convert : expr -> lamcom
end

module type Interpreter = sig 
  open Lambdaast

  (** [lamcom c] evaluates a lambda calculus expression until it gets stuck*)
  val eval : lamcom -> lamcom
end 

module type Repl = sig 
  (** [repl] Is a REPL interpreter for LML*)
  val repl : unit -> unit
end



module Make (C : Converter) : Repl = struct
  open Ast
  open Pprint
  open Parse
  open Pprint
  open Interp

  let eval_and_print (e : expr) : unit = let e' = C.convert e in 
    print_endline ("-> " ^ (string_of_exp e'));
    print_endline ("-> " ^ (e' |> eval |> string_of_exp))

  let rec eval_and_loop () = 
    print_endline "--------------------------------------------------------------------------------";
    print_string ">  ";
    (* Exception handling based on https://ocaml.org/learn/tutorials/error_handling.html  *)
    let input = read_line () in
    begin
      try input |> parse |> eval_and_print with
        e -> let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
        print_endline ("Error evaluating or parsing: " ^ msg ^ stack)
    end;
    eval_and_loop ()

  let repl () = eval_and_loop ()
end