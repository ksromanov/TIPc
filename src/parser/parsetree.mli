(* Parse tree of TIP, directly resulting from parsing *)
type binop =
    | Plus
    | Minus
    | Times
    | Div
    | Greater
    | Equal

type ident = string

type record = (ident*expression) list

and  expression =
    | Int of int
    | Id of ident
    | Binop of expression * binop * expression
    | Parenthesized of expression
    | DirectApply of ident*expression list
    | ComputedApply of expression*expression list
    | Alloc of expression
    | Reference of ident
    | DeReference of expression
    | Null
    | Record of record

type statement =
    | Assignment of ident * expression
    | Output of expression
    | Error of expression
    | If of expression*statement*statement option
    | While of expression*(statement list)
    | Store of expression * expression (* Store value in a memory cell referenced by pointer *)
    | DirectRecordWrite of ident*ident*expression
    | IndirectRecordWrite of expression*ident*expression

type argument = ident

type func = { args : argument list; var_blocks : ident list list; stmts : statement list; ret_expr : expression }
type program = func list
