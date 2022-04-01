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
%type <string> stmt
%type <int> expr
%type <int> record_field

%%
program: p = list(func); EOF { p }

func: 
|   name = IDENT; LPAREN; args = separated_list(COMMA, IDENT); RPAREN;
    LBRACE; var_bs = list(vars_block); stmts = list(stmt); KRETURN; expr; SEMI; RBRACE
    { List.length args + List.length var_bs + String.length name + List.length stmts }

vars_block: KVAR; vs = separated_nonempty_list(COMMA, IDENT); SEMI { vs }

stmt:
| id = IDENT; ASSIGN; expr; SEMI { id }
| KOUTPUT expr SEMI { "KOUTPUT" }
| KERROR expr SEMI { "KERROR" }
| KIF; LPAREN; expr; RPAREN; 
  stmt KELSE stmt { "if" }
| KIF; LPAREN; expr; RPAREN; 
  stmt { "iff" }
| KWHILE; LPAREN; expr; RPAREN;
  LBRACE; list(stmt); RBRACE { "while" }
| TIMES; expr; ASSIGN; expr ; SEMI { "indirect" }
| IDENT DOT IDENT ASSIGN expr ; SEMI { "record access" }
| LPAREN TIMES expr RPAREN DOT IDENT ASSIGN expr ; SEMI { "star access" }
| LBRACE; list(stmt); RBRACE { "block" }

record_field:
  field = separated_pair(IDENT, COLON, expr) { String.length (fst field) }

expr:
| IDENT {0}
| n = INT { n }
| expr; PLUS ; expr { 1 }
| expr; MINUS ; expr { 1 }
| MINUS; n = INT { -n }
| expr; TIMES ; expr { 1 }
| expr; DIV ; expr { 1 }
| expr; GREATER ; expr { 1 }
| expr; EQUAL ; expr { 1 }
| LPAREN; e = expr; RPAREN { e }
| KINPUT { 88 }
| IDENT; LPAREN; args = separated_list( COMMA, expr); RPAREN
  { List.length(args) } // direct or indirect function call
| expr; LPAREN; args = separated_list( COMMA, expr); RPAREN
  { List.length(args) }  // definitely indirect function call
| KALLOC; e = expr { 3 + e }
| AMPERSAND; expr { 4 }
| TIMES; expr { 6 }
| KNULL { 5 }
| LBRACE; separated_nonempty_list( COMMA, record_field); RBRACE { 8 }
| expr; DOT; IDENT { 9 }
