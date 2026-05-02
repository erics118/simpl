{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white     { read lexbuf }
  | "let"     { LET }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fun"     { FUN }
  | "->"      { ARROW }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true"    { BOOL true }
  | "false"   { BOOL false }
  | "fst"     { FST }
  | "snd"     { SND }
  | "Left"    { LEFT }
  | "Right"   { RIGHT }
  | "match"   { MATCH }
  | "with"    { WITH }
  | ","       { COMMA }
  | "|"       { PIPE }  
  | id        { ID (Lexing.lexeme lexbuf) }
  | "="       { EQ }
  | "<>"      { NEQ }
  | "<"       { LT }
  | "<="      { LEQ }
  | ">"       { GT }
  | ">="      { GEQ }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | eof       { EOF }
