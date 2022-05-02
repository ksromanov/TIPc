(* A normal form with type information *)
open Anf

type typeVariable = int [@@deriving show]

(* Типы разных объектов программы - функций, аргументов, переменных *)
type entityType =
  | Int
  | Pointer of entityType
  | Arrow of entityType list * entityType (* type of a function *)
  | TypeVar of typeVariable
  | Mu of typeVariable * entityType (* пока не используется! *)
[@@deriving show]

type func = {
  name : string;
  args : (argument * entityType) list;
  var_blocks : (string * entityType) list list;
  stmts : statement list;
  ret_expr : atomic_expression;
  (* Additional type information *)
  return_type : entityType;
  temporary_vars : (int * entityType) list;
}
[@@deriving show]

type program = func list [@@deriving show]
