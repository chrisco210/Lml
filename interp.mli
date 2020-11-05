open Lambdaast

(** [lamcom c] evaluates a lambda calculus expression until it gets stuck*)
val eval : lamcom -> lamcom