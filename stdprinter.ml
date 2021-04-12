open Lambdaast
open Pprint


let display_help () = 
  print_endline ("
The ML language is defined with the following grammar:
bop ::= + | - | * | / | < | > | <= | >= | = | != | :: | && | || 
uop ::= ~ | ~- | !

e ::= (e)
      | let x = e1 in e2
      | let rec x = e1 in e2
      | fun x1 ... xn -> e
      | e1 e2
      | if e1 then e2 else e3
      | while e1 do e2 done
      | break
      | continue
      | e1; e2
      | ref e
      | e1 := e2
      | get
      | set e
      | x
      | []                   <-- Nil
      | L x . e
      | (e1, e2, ... en)
      | e#n
      | e1 bop e2
      | uop e
      | n
      | unit                   <-- Unit
      | true
      | false

These generally do what you would expect them to do.  The `::` opera-
tor is a cons operator to append to a list, `e#n` extracts the `n`th 
item from a tuple `e`. For unary operators,  `~` is logical negation,
while `~-` is integer negation, and `!` is dereferencing.

You can do OCaml style comments using `(* ocaml comment *)` syntax.\n")

let display_hello () = 
  print_endline ("
---------------------------------------------------------------------
                               LML REPL                                    
---------------------------------------------------------------------
          Press ^C to quit, type `^` to repeat last command
See README.md or type `help` for syntax help and some examples to run\n")

let display_expr (i : lamcom) (f : lamcom) : unit = 
  print_string ("-> ");
  print_string ((string_of_exp i) ^ "\n");
  print_string("-> ");
  print_string ((string_of_exp f) ^ "\n")

let display_prompt () = 
  print_endline ("---------------------------------------------------------------------");
  print_string (">  ")

let display_error (msg : string) (stk : string) = 
  print_string ("Error evaluating or parsing: " ^ msg ^ stk ^ "\n")
