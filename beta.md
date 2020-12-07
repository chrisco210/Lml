# Vision
Our goal for this project is to create a system that takes a standard functional
language similar to OCaml and converts it to lambda calculus. The system then
takes the lambda calculus and evaluates it to a lambda calculus value, which is
the output.

# Status

As of now, we finished the simpler conversions between the two languages, such
as addition and functions. We also have created a parser and lexer, which takes
input and converts it into an AST. In addition, we have an evaluater that takes 
thelambda calculus and outputs a value. In the second sprint, we added tuples 
and lists as additional data structures. We also added continuations, and using 
that we implemented while loops and sequences.

# Next Steps

Our plan for the next sprint is to implement references. We plan to have one 
mutable "slot," as opposed to OCaml-like references. We would also like to 
improve the UI of our system, as it is a bit lacking at the moment. Finally, we 
want to implement breaks and continues, which we think will probably be the 
hardest part of the project.