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
%token ASSIGN
%token LBRACE
%token RBRACE

%token EOF

%start <int list> program
%type <int> func
%type <string list> args
%type <string list> stmt
%type <string list> vars
%type <string list> var_list
%type <int> expr
%type <int> record_field
%type <int list> record_fields

%%
program:
| f = func; p = program { f::p }
| EOF { [] }

func: 
|   name = IDENT; LPAREN; a = args; RPAREN;
    LBRACE; vars; stmt; KRETURN; expr; SEMI; RBRACE
    { List.length a + String.length name }
|   name = IDENT; LPAREN; a = args; RPAREN;
    LBRACE; stmt; KRETURN; expr; SEMI; RBRACE
    { List.length a + String.length name }

args:
| arg = IDENT; COMMA; a = args { arg::a }
| arg = IDENT { [arg] }
| { [] }

vars:
| KVAR; vars = var_list; SEMI { vars }
| { [] } 

var_list:
| id = IDENT; COMMA; vars = var_list { id :: vars }
| id = IDENT { [id] }

stmt:
| id = IDENT; ASSIGN; expr; SEMI { [id] }
| KOUTPUT expr SEMI { ["KOUTPUT"] }
| s1 = stmt; SEMI; s = stmt { s1 @ s }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt RBRACE;
  KELSE LBRACE; stmt ; RBRACE { ["if"] }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt RBRACE { ["iff"] }
| KWHILE; LPAREN; expr; RPAREN;
  LBRACE; stmt RBRACE { ["while"] }
| TIMES; expr; ASSIGN; expr ; SEMI { ["indirect"] }
| IDENT DOT IDENT ASSIGN expr ; SEMI { ["record access"] }
| LPAREN TIMES expr RPAREN DOT IDENT ASSIGN expr ; SEMI { ["star access"] }
| { [] }

expr:
| IDENT {0}
| n = INT { n }
| expr; PLUS ; expr { 1 }
| expr; MINUS ; expr { 1 }
| expr; TIMES ; expr { 1 }
| expr; DIV ; expr { 1 }
| expr; GREATER ; expr { 1 }
| expr; EQUAL ; expr { 1 }
| LPAREN; e = expr; RPAREN { e }
| IDENT; LPAREN; args; RPAREN { 2 } // direct or indirect function call
| expr; LPAREN; args; RPAREN { 2 }  // definitely indirect function call
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
