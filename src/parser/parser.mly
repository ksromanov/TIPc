(* Draft skeleton of TIP language grammar *)
%{
  open Parsetree
%}

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

%start <Parsetree.program> program

%%
program: p = list(func); EOF { p }

func:
|   name = IDENT; LPAREN; args = separated_list(COMMA, IDENT); RPAREN;
    LBRACE; var_blocks = list(vars_block); stmts = list(stmt); KRETURN; ret_expr = expr; SEMI; RBRACE
    { { name; args; var_blocks; stmts; ret_expr } }

vars_block: KVAR; vs = separated_nonempty_list(COMMA, IDENT); SEMI { vs }

stmt:
| id = IDENT; ASSIGN; e = expr; SEMI { Assignment (id, e) }
| KOUTPUT; e = expr; SEMI { Output e }
| KERROR; e = expr; SEMI { Error e }
| KIF; LPAREN; cond = expr; RPAREN;
  thn = stmt; KELSE; els = stmt;
  { If (cond, thn, Some els) }
| KIF; LPAREN; cond = expr; RPAREN;
  thn = stmt;
  { If (cond, thn, None) }
| KWHILE; LPAREN; cond = expr; RPAREN;
  LBRACE; body = list(stmt); RBRACE;
  { While (cond, body) }
| TIMES; p = expr; ASSIGN; value = expr ; SEMI { Store (p, value) }
| r = IDENT DOT field = IDENT ASSIGN value = expr ; SEMI
  { DirectRecordWrite (r, field, value) }
| LPAREN TIMES; p = expr; RPAREN DOT; field = IDENT; ASSIGN;
  value = expr ; SEMI { IndirectRecordWrite (p, field, value) }
| LBRACE; block = list(stmt); RBRACE { Block block }

record_field:
  field = separated_pair(IDENT, COLON, expr) { field }

expr:
| id = IDENT { Id id }
| n = INT { Int n }
| a = expr; PLUS ; b = expr { Binop (a, Plus, b) }
| a = expr; MINUS ; b = expr { Binop (a, Minus, b) }
| MINUS; n = INT { Int (-n) }
| a = expr; TIMES ; b = expr { Binop (a, Times, b) }
| TIMES; e = expr { DeReference e }
| a = expr; DIV ; b = expr { Binop (a, Div, b) }
| a = expr; GREATER ; b = expr { Binop (a, Greater, b) }
| a = expr; EQUAL ; b = expr { Binop (a, Equal, b) }
| LPAREN; e = expr; RPAREN { e }
| KINPUT { Input }
| f = IDENT; LPAREN; args = separated_list( COMMA, expr); RPAREN
  { DirectApply (f, args) } // direct or indirect function call
| f = expr; LPAREN; args = separated_list( COMMA, expr); RPAREN
  { ComputedApply (f, args) }  // definitely indirect function call
| KALLOC; e = expr { Alloc e }
| AMPERSAND; id = IDENT { Reference id }
| KNULL { Null }
| LBRACE; fields = separated_nonempty_list( COMMA, record_field); RBRACE
  { Record fields }
| r = expr; DOT; field = IDENT { FieldRead (r, field) }
