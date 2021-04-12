(* Main interface for running Lml online *)
open Lambdaast
open Ast
open Pprint
open Parse
open Pprint
open Interp
open Convert
open Js_of_ocaml


let eval_and_str (e : expr) : string * string = 
  let i = convert e in 
  let f = eval i in 
  (string_of_exp i, string_of_exp f) 

let _ = 
  Js.export "lml"
    (object%js
      method interp str = 
        let str = str |> Js.to_string in 
        let parsed = parse str in 
        let c = convert parsed in 
        let final = eval c in 
        final |> string_of_exp |> Js.string

      val test = 0
      method add a b = a + b

    end)
