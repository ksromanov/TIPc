(* Description of a-normal form for TIPc *)
type binop = Plus | Minus | Times | Div | Greater | Equal [@@deriving show]
type ident = Ident of string | Temporary of int [@@deriving show]

type record = (ident * atomic_expression) list [@@deriving show]
and atomic_expression = Int of int | Id of ident | Null [@@deriving show]

and complex_expression =
  | Binop of atomic_expression * binop * atomic_expression
  | Input
  | Apply of ident * atomic_expression list
  | ComputedApply of ident * atomic_expression list
  | Alloc of atomic_expression
  | Reference of ident
  | DeReference of atomic_expression
  | Record of record
  | FieldRead of atomic_expression * ident
[@@deriving show]

and expression = Atomic of atomic_expression | Complex of complex_expression
[@@deriving show]

type statement =
  | Assignment of ident * expression
  | Output of atomic_expression
  | Error of atomic_expression
  | If of atomic_expression * statement * statement option
  | While of atomic_expression * statement list
  | Store of
      ident
      * expression (* Store value in a memory cell referenced by pointer *)
  | DirectRecordWrite of ident * ident * expression
  | Block of statement list
[@@deriving show]

type argument = ident [@@deriving show]

type func = {
  name : string;
  args : argument list;
  var_blocks : string list list;
  stmts : statement list;
  ret_expr : atomic_expression;
}
[@@deriving show]

type program = func list [@@deriving show]
