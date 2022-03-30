(* Draft skeleton of TIP language grammar *)
%token KALLOC
%token KINPUT
%token KWHILE
%token KIF
%token KELSE
%token KVAR
%token KRETURN
%token KNULL
%token KOUTPUT
%token KERROR
%token <int> INT
%token <string> IDENT

%token GREATER
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token AMPERSAND

%left GREATER
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */

%token LPAREN
%token RPAREN
%token SEMI
%token COLON
%token COMMA
%token DOT
%token EQUAL
%token LBRACE
%token RBRACE

%token EOF

%start <int list> program
%type <int> func
%type <string list> args
%type <string list> stmt
%type <string list> vars
%type <int> expr

%%
program:
| f = func; p = program { f::p }
| { [] }

func: 
    name = IDENT; LPAREN; a = args; RPAREN;
    LBRACE; stmt; KRETURN; expr; RBRACE
    { List.length a + String.length name }

args:
| arg = IDENT; COMMA; a = args { arg::a }
| arg = IDENT { [arg] }

vars:
| KVAR; var_list; SEMI { var_list }
| { [] } 

var_list:
| id = IDENT; COMMA; var_list { id :: var_list }
| { [] }

stmt:
| id = IDENT; EQUAL; expr; SEMI { [id] }
| KOUTPUT expr { ["KOUTPUT"] }
| s1 = stmt; SEMI; s = stmt { s1 @ s }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt RBRACE;
  KELSE LBRACE; stmt ; RBRACE { ["if"] }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt RBRACE { ["iff"] }
| KWHILE; LPAREN; expr; RPAREN;
  LBRACE; stmt RBRACE { ["while"] }
| TIMES; expr; EQUAL; expr ; SEMI { ["indirect"] }
| IDENT DOT IDENT EQUAL expr ; SEMI { ["record access"] }
| LPAREN TIMES expr RPAREN DOT IDENT EQUAL expr ; SEMI { ["star access"] }

expr:
| IDENT {0}
| n = INT { n }
| expr; PLUS ; expr { 1 }
| expr; MINUS ; expr { 1 }
| expr; TIMES ; expr { 1 }
| expr; DIV ; expr { 1 }
| LPAREN; e = expr; RPAREN { e }
| IDENT; LPAREN; args; RPAREN { 2 } // function call
| KALLOC; expr { 3 }
| AMPERSAND; expr { 4 }
| TIMES; expr { 6 }
| KNULL { 5 }
| RBRACE; record_fields; LBRACE { 8 }
| expr; DOT; IDENT { 9 }

record_fields:
| f = record_field; COMMA; fs = record_fields { f :: fs }
| { [] }

record_field:
| IDENT; COLON; expr { 8 }
