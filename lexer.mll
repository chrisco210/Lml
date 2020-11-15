{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" {IF}
  | "else" {ELSE}
  | "then" {THEN}
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | "<" { LT }
  | ">" { GT }
  | "*" { TIMES }
  | "-" { MINUS }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUALS }
  | "!=" { NEQ }
  | "L" { LAMBDA }
  | "." { PERIOD }
  | "let" { LET }
  | "let rec" { LETREC } 
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }