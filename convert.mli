open Ast
open Lambdaast

(** [convert e] is a lambda calculus translation of an expression e*)
val convert : expr -> lamcom