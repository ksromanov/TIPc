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
%type <string> stmt
%type <string list> stmt_list
%type <string list> var_list
%type <string list list> vars_blocks
%type <int> expr
%type <int> record_field
%type <int list> record_fields

%%
program:
| f = func; p = program { f::p }
| EOF { [] }

func: 
|   name = IDENT; LPAREN; a = args; RPAREN;
    LBRACE; vars_blocks; stmt_list; KRETURN; expr; SEMI; RBRACE
    { List.length a + String.length name }

args:
| arg = IDENT; COMMA; a = args { arg::a }
| arg = IDENT { [arg] }
| { [] }

vars_blocks:
| KVAR; vars = var_list; SEMI  vbs = vars_blocks { vars :: vbs }
| { [] }

var_list:
| id = IDENT; COMMA; vars = var_list { id :: vars }
| id = IDENT { [id] }

stmt:
| id = IDENT; ASSIGN; expr; SEMI { id }
| KOUTPUT expr SEMI { "KOUTPUT" }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt_list RBRACE;
  KELSE LBRACE; stmt_list ; RBRACE { "if" }
| KIF; LPAREN; expr; RPAREN; 
  LBRACE; stmt_list RBRACE { "iff" }
| KWHILE; LPAREN; expr; RPAREN;
  LBRACE; stmt_list RBRACE { "while" }
| TIMES; expr; ASSIGN; expr ; SEMI { "indirect" }
| IDENT DOT IDENT ASSIGN expr ; SEMI { "record access" }
| LPAREN TIMES expr RPAREN DOT IDENT ASSIGN expr ; SEMI { "star access" }

stmt_list:
| s = stmt; ss = stmt_list { s::ss }
| { [] }

expr_list:
| e = expr; COMMA; es = expr_list { e::es }
| e = expr { [e] }

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
| KINPUT { 88 }
| IDENT; LPAREN; expr_list; RPAREN { 2 } // direct or indirect function call
| expr; LPAREN; expr_list; RPAREN { 2 }  // definitely indirect function call
| KALLOC; e = expr { 3 + e }
| AMPERSAND; expr { 4 }
| TIMES; expr { 6 }
| KNULL { 5 }
| LBRACE; record_fields; RBRACE { 8 }
| expr; DOT; IDENT { 9 }

record_fields:
| f = record_field; COMMA; fs = record_fields { f :: fs }
| f = record_field { [f] }

record_field:
| IDENT; COLON; expr { 8 }
