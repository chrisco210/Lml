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



module Make (C : Converter) : Repl = struct
  open Ast
  open Pprint
  open Parse
  open Pprint
  open Interp

  let eval_and_print (e : expr) : unit = let e' = C.convert e in 
    ANSITerminal.(print_string [green] ("-> "));
    ANSITerminal.(print_string [default] ((string_of_exp e') ^ "\n"));
    ANSITerminal.(print_string [green] ("-> "));
    ANSITerminal.(print_string [default] ((e' |> eval |> string_of_exp) ^ "\n"))

  let help () =
    ANSITerminal.(print_string [yellow] ("
The ML language is defined with the following grammar:

bop ::= + | - | * | < | > | <= | >= | = | != | ::
uop ::= ~ | ~- | !

e ::= (e)

      | let x = e1 in e2
      | let rec x = e1 in e2
      | fun x1 ... xn -> e
      | e1 e2
      | if e1 then e2 else e3
      | while e1 do e2 done
      | e1; e2
      | ref e
      | e1 := e2
      | x
      | []                   <-- Nil
      | L x . e
      | let x = e
      | (e1, e2)
      | e#n
      | e1 bop e2
      | uop e
      | n
      | ()                   <-- Unit
      | true
      | false

These generally do what you would expect them to do.  The `::` opera-
tor is a cons operator to append to a list, `e#n` extracts the `n`th 
item from a tuple `e`. For unary operators,  `~` is logical negation,
while `~-` is integer negation, and `!` is dereferencing.

You can do OCaml style comments using `(* ocaml comment *)` syntax.\n"))

  let rec eval_and_loop (last:string) = 
    ANSITerminal.(print_string [cyan] ("---------------------------------------------------------------------\n"));
    ANSITerminal.(print_string [green] (">  "));
    (* Exception handling based on https://ocaml.org/learn/tutorials/error_handling.html  *)
    let input = read_line () in
    begin
      match input with
      | "help" -> help ()
      | "^" -> ANSITerminal.(print_string [green] ("-> ")); print_endline last;
        begin
          try last |> parse |> eval_and_print with
            e -> let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            ANSITerminal.(print_string [red] ("Error evaluating or parsing: " ^ msg ^ stack ^ "\n"))
        end
      | _ -> 
        begin
          try input |> parse |> eval_and_print with
            e -> let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            ANSITerminal.(print_string [red] ("Error evaluating or parsing: " ^ msg ^ stack ^ "\n"))
        end
    end;
    eval_and_loop input

  let repl () = 
    ANSITerminal.(print_string [cyan] ("
---------------------------------------------------------------------
                               LML REPL                                    
---------------------------------------------------------------------
          Press ^C to quit, type `^` to repeat last command
See README.md or type `help` for syntax help and some examples to run\n"));
    eval_and_loop ""
end