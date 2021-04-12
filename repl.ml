(** This module contains a functor for creating a repl with a given converter*)

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

module type Printer = sig 
  open Lambdaast

  val display_help : unit -> unit

  val display_hello : unit -> unit

  (** [display_expr i f] displays an expression of intermediate form [i] and 
      final form [f]*)
  val display_expr : lamcom -> lamcom -> unit

  val display_prompt : unit -> unit

  (** [display_error err stk] displays an error with message [err] and stack [stk] *)
  val display_error : string -> string -> unit
end


module Make (C : Converter) (P : Printer) : Repl = struct
  open Ast
  open Pprint
  open Parse
  open Pprint
  open Interp

  let eval_and_print (e : expr) : unit = 
    let i = C.convert e in 
    let f = eval i in 
    P.display_expr i f 

  let help () = P.display_help ()

  let rec eval_and_loop (last:string) = 
    P.display_prompt ();
    (* Exception handling based on https://ocaml.org/learn/tutorials/error_handling.html  *)
    let input = read_line () in
    begin
      match input with
      | "help" -> help (); eval_and_loop last
      | "^" -> 
        P.display_prompt ();
        begin
          try last |> parse |> eval_and_print with
            e -> let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            P.display_error msg stack
        end;
        eval_and_loop last
      | _ -> 
        begin
          try input |> parse |> eval_and_print with
            e -> let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            P.display_error msg stack
        end;
        eval_and_loop input
    end


  let repl () = 
    P.display_hello ();
    eval_and_loop ""
end