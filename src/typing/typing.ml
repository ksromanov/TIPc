(* Type inference algorithm for TIPc language.
   We start from ANF representation. *)

open Anf

type function_name = string
type typeVariable = int

type varType =
  | Int
  | Pointer of varType
  | Arrow of varType list * varType
  | TypeVar of typeVariable (* фактически заменяется на Id of typeable? *)
  | Mu of typeVariable * varType (* вообще не используется... *)
  | Id of typeable

and typeable =
  | Function of function_name
  | Variable of function_name * ident (* Both argument and variable *)

module Typeables = Set.Make (struct
  type t = varType

  let compare = compare
end)

let collect_typeables program =
  let collect_typeables_func set { name; args; var_blocks; stmts; _ } =
    let collect_typeables_var_blocks set var_blocks =
      let collect_typeables_var set = function
        | Ident _ as id -> Typeables.add (Id (Variable (name, id))) set
        | Temporary _ -> failwith "Temporary in var block in collect_typeables"
      in

      List.fold_left collect_typeables_var set (List.flatten var_blocks)
    in
    let rec collect_typeables_stmt set = function
      | Assignment (id, _) -> Typeables.add (Id (Variable (name, id))) set
      | Output _ -> set
      | Error _ -> set
      | If (_, thn, Some els) ->
          collect_typeables_stmt (collect_typeables_stmt set thn) els
      | If (_, thn, None) -> collect_typeables_stmt set thn
      | While (_, body) -> List.fold_left collect_typeables_stmt set body
      | Store (id, _) -> Typeables.add (Id (Variable (name, id))) set
      | DirectRecordWrite (id, _, _) ->
          Typeables.add (Id (Variable (name, id))) set
      | Block body -> List.fold_left collect_typeables_stmt set body
    in

    let set = Typeables.add (Id (Function name)) set in
    let set = collect_typeables_var_blocks set var_blocks in
    let set =
      List.fold_left
        (fun set arg -> Typeables.add (Id (Variable (name, arg))) set)
        set args
    in
    List.fold_left collect_typeables_stmt set stmts
  in
  List.fold_left collect_typeables_func Typeables.empty program

let infer program =
  let typeables = collect_typeables program in
  Typeables.cardinal typeables

module UnionFindX = struct
  type elem = typeable
  type store = (typeable, typeable) Hashtbl.t

  let data : store = Hashtbl.create 10
  let add e = Hashtbl.add data e e

  let rec find e =
    match Hashtbl.find data e with r when r = e -> r | r -> find r

  let union a b =
    let b_root = find b in
    let rec update_root e r =
      match Hashtbl.find data e with
      | r' when r' = e -> Hashtbl.add data e r
      | r' ->
          Hashtbl.add data e r;
          update_root r' r
    in
    update_root a b_root
end
