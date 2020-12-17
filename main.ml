open Repl
open Fileinterp

module DefaultRepl = Repl.Make(Convert)
module RawRepl = Repl.Make(Convertraw)

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
