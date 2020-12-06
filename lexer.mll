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
  | "while" {WHILE}
  | "do" {DO}
  | "done" {DONE}
  | "continue" {CONTINUE}
  | "break" {BREAK}
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
  | "&&" { AND }
  | "||" { OR }
  | "~" { NOT }
  | "~-" { NEG }
  | "L" { LAMBDA }
  | "." { PERIOD }
  | "," { COMMA }
  | "#" { POUND }
  | "let" { LET }
  | "let rec" { LETREC } 
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "::" { CONS }
  | "hd" { HD }
  | "tl" { TL }
  | "[]" { NIL }
  | "unit" { UNIT }
  | ";" {SEMICOLON}
  | "(*" { comment 0 lexbuf } 
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
(*Comments are based on the 3110 A5 RML lexer*)
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 0 then read lexbuf else comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof  { EOF }
  | _    { comment depth lexbuf }
