{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
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
  | "L" {LAMBDA}
  | "\\." {PERIOD}
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }