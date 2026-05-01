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
  | white   { read lexbuf }
  | "let"   { LET }
  | "in"    { IN }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id      { ID (Lexing.lexeme lexbuf) }
  | "="     { EQ }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | eof     { EOF }
