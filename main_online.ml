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
      (** [interp str] is an object with the converted and final results of the 
          interpreter*)
      method interp str = 
        let str = str |> Js.to_string in 
        let parsed = parse str in 
        let converted = convert parsed in 
        let final = eval converted in 
        object%js 
          val convert = converted |> string_of_exp |> Js.string
          val result = final  |> string_of_exp |> Js.string
        end
    end)
