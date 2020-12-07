# Vision
Our goal for this project is to create a system that takes a standard functional
language similar to OCaml and converts it to lambda calculus, extended with 
integers, booleans, if statements, and unit. The system then
takes the lambda calculus and evaluates it to a lambda calculus value, which is
the output.

The LML language is defined as follows, with everything behaving as you would
expect in OCaml, with the addition of being able to write pure lambda calculus
in addition to normal operations.
```
bop ::= + | - | * | < | > | <= | >= | = | != | :: | && | ||
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
      | x
      | []                   <-- Nil
      | is_nil e
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
```

The target language is defined as follows:
```
bop ::= + | - | * | < | > | <= | >= | = | != 
uop ::= ~ | ~- | !
e ::= x
      | n
      | b
      | ()
      | $i <-- This is a De Bruijn variable number
      | if e1 then e2 else e3
      | L . e
      | e1 e2
      | if e1 then e2 else e3
      | e1 bop e2
      | uop e
```

# Status

As of now, we have implemented a parser and lexer using menhir, implemented 
list and tuple data structures using church encodings, implemented multi 
argument functions, including recursively defined functions, sequences, and 
while loops.  

In addition to this, we have implemented a call by name lambda calculus 
interpreter.  This interpreter is not very optimized, but we felt that we would
rather focus on implementing more features to translate than focus on our 
interpreter for the first few sprints.  We chose a call by name interpreter 
because we found it was less likely to get stuck, allowing our encodings to be
more flexible than if we used a call by value interpreter.

To prepare for implementing more imperative features in our release sprint, 
in our beta sprint we implemented a CPS translation for our lambda calculus.  
This makes it much easier to specify control flow for imperative features like
sequences and while loops, and will also make keeping track of references much 
easier when we implement them.  The translation works in two layers. We first 
translating the features that don't care as much about evaluation order, like 
data structures and functions, into an intermediate form.  Once we have the
intermediate form, we are then able to use the standard CPS translation, in 
addition to our own CPS translations of imperative features.

For our interface, we have a basic REPL interface that shows translated lambda
calculus as well as the evaluation result:
```
--------------------------------------------------------------------------------
                                    LML REPL                                    
--------------------------------------------------------------------------------
Press ^C to quit, see README.md for syntax help and some examples to run
--------------------------------------------------------------------------------
>  let x = 4110 in x 
-> ((L . ((L . ($0 (L . (L . ((L . ($0 $2)) $0))))) (L . ((L . ($0 4110)) (L . (($1 $0) $2)))))) (L . $0))
-> 4110
--------------------------------------------------------------------------------
```
As well as the ability to run programs stored in files.  

# Examples

Here are some interesting examples:

A GCD function:
```
let rec mod = fun a b -> 
  if a < b then a else
    mod (a - b) b
in
let rec gcd = fun a b ->
  if a = 0 then b else
  if b = 0 then a else
    let r = mod a b in
    gcd b r 
in
gcd 55 10
```

Fun with folds:
```
let rec fold_left = fun op acc lst ->
  if is_nil lst then acc else fold_left op (op acc (hd lst)) (tl lst) 
in 
let map_rev = fun f l -> fold_left (fun a x -> (f x)::a) [] l in
let theList = (1::2::3::[]) in
let mapped = map_rev (fun x -> x + 1) theList in 
fold_left (fun a b -> a + b) 0 mapped
```

An example with a smaller translation output:
```
let add = fun x y -> x + y in
add 1 2
```
Gives the (ugly because of CPS) output:
```
-> ((L . ((L . ($0 (L . (L . ((L . ((L . ((L . ($0 $4)) (L . ((L . ($0 1)) (L . (($1 $0) $2)))))) (L . ((L . ($0 2)) (L . (($1 $0) $2)))))) $0))))) (L . ((L . ($0 (L . (L . ((L . ($0 (L . (L . ((L . ((L . ($0 $6)) (L . ((L . ($0 $4)) (L . ($2 $1+$0)))))) $0))))) $0))))) (L . (($1 $0) $2)))))) (L . $0))
-> 3
```


# Next Steps

Our plan for the next sprint is to implement references, and improve the speed 
of our interpreter.  We would also like to improve the UI of our system, as it 
is a bit lacking at the moment. Finally, we want to implement breaks and 
continues of while loops, which we think will be fairly difficult to do.