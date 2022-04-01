(* Parse tree of TIP, directly resulting from parsing *)
type binop = Plus | Minus | Times | Div | Greater | Equal [@@deriving show]
type ident = string [@@deriving show]

type record = (ident * expression) list [@@deriving show]

and expression =
  | Int of int
  | Id of ident
  | Binop of expression * binop * expression
  | Input
  | Parenthesized of expression
  | DirectApply of ident * expression list
  | ComputedApply of expression * expression list
  | Alloc of expression
  | Reference of ident
  | DeReference of expression
  | Null
  | Record of record
  | FieldRead of expression * ident
[@@deriving show]

type statement =
  | Assignment of ident * expression
  | Output of expression
  | Error of expression
  | If of expression * statement * statement option
  | While of expression * statement list
  | Store of
      expression
      * expression (* Store value in a memory cell referenced by pointer *)
  | DirectRecordWrite of ident * ident * expression
  | IndirectRecordWrite of expression * ident * expression
  | Block of statement list
[@@deriving show]

type argument = ident [@@deriving show]

type func = {
  name : string;
  args : argument list;
  var_blocks : ident list list;
  stmts : statement list;
  ret_expr : expression;
}
[@@deriving show]

type program = func list [@@deriving show]
