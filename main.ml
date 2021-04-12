open Repl
open Fileinterp

open Colorprinter

module DefaultRepl = Repl.Make(Convert) (Colorprinter)
module RawRepl = Repl.Make(Convertraw) (Colorprinter)

type args = 
  | File of string 
  | Repl
  | Raw


let args (pargs : string list) : args = 
  match pargs with 
  | _::"raw"::[] -> Raw
  | _::infile::[] -> File infile
  | _ -> Repl

let _ = 
  match args (Array.to_list Sys.argv) with
  | File path ->  path |> interp_file
  | Repl -> DefaultRepl.repl ()
  | Raw -> RawRepl.repl ()
