(* TIP (Tiny Imperative Programming Language) lexer.
   Fully passes examples in https://github.com/cs-au-dk/TIP
   as of 31 March 2022.

   Does not support nested multiline comments. *)
{
  open Parser

  exception Error of string
}

let newline = ('\013'* '\010')
let ws = [' ' '\009' '\012']
let int_literal = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
  | ws + { token lexbuf }
  | newline { token lexbuf }
  | "alloc" { KALLOC }
  | "input" { KINPUT }
  | "while" { KWHILE }
  | "if" { KIF }
  | "else" { KELSE }
  | "var" { KVAR }
  | "return" { KRETURN }
  | "null" { KNULL }
  | "output" { KOUTPUT }
  | "error" { KERROR }
  | int_literal as i { INT (int_of_string i)}
  | "==" { EQUAL }
  | "="  { ASSIGN }
  | ">"  { GREATER }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { TIMES }
  | "/*" { multi_line_comment lexbuf }
  | '/'  { DIV }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ';'  { SEMI }
  | ':'  { COLON }
  | ','  { COMMA }
  | '.'  { DOT }
  | '&'  { AMPERSAND }
  | ident as i { IDENT i }
  | eof { EOF }
  | "//" { eol_comment lexbuf }
  | _ as c
      { Printf.printf "Unexpected character '%c', code %d, skipping...\n" c (Char.code c);
        token lexbuf }
and eol_comment = parse
  | '\010' { token lexbuf }
  | _ { eol_comment lexbuf }
and multi_line_comment = parse
  | "*/" { token lexbuf }
  | _ { multi_line_comment lexbuf }
