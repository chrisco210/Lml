open Lambdaast

exception UnboundVariable of var
exception TypeError of string

(** [lamcom c] evaluates a lambda calculus expression until it gets stuck*)
val eval : lamcom -> lamcom

