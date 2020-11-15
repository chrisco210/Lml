# Vision
Our goal for this project is to create a system that takes a standard functional
language similar to OCaml and converts it to lambda calculus. The system then
takes the lambda calculus and evaluates it to a lambda calculus value, which is
the output.

# Status

As of now, we finished the simpler conversions between the two languages, such
as addition and functions. We also have created a parser and lexer, which takes
input and converts it into an AST. Finally, we have an evaluater that takes the
lambda calculus and outputs a value.

# Next Steps

Our plan for the next sprint is to implement data structures such as lists and
tuples. We also hope to implement references in this part. After that, if we
have time, we would like to implement control flow with sequences, breaks, and
continues.