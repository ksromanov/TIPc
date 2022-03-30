(* Draft version of TIP language lexer *)
{
  open Parser

  exception Error of string

}

let newline = ('\013'* '\010')
let ws = [' ' '\009' '\012']
let int_literal = ['0'-'9']*
let ident = ['a'-'z' 'A'-'Z']+

rule token = parse
  | ws +
      { token lexbuf }
  | "alloc"
      { KALLOC }
  | "input"
      { KINPUT }
  | "while"
      { KWHILE }
  | "if"
      { KIF }
  | "else"
      { KELSE }
  | "var"
      { KVAR }
  | "return"
      { KRETURN }
  | "null"
      { KNULL }
  | "output"
      { KOUTPUT }
  | "error"
      { KERROR }
  | '-'? int_literal as i
      { INT (int_of_string i)}
  | "==" { EQUAL }
  | ">"  { GREATER }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { TIMES }
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
  | ident
      { IDENT ident }
  | eof { EOF }
