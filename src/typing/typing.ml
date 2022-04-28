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
  | Entity of typeable

and typeable =
  | Function of function_name
  | Retval of function_name
  | Variable of function_name * ident (* Both argument and variable *)

module Typeables = Set.Make (struct
  type t = varType

  let compare = compare
end)

let collect_typeables program =
  let collect_typeables_func set { name; args; var_blocks; stmts; _ } =
    let collect_typeables_var_blocks set var_blocks =
      let collect_typeables_var set = function
        | Ident _ as id -> Typeables.add (Entity (Variable (name, id))) set
        | Temporary _ -> failwith "Temporary in var block in collect_typeables"
      in

      List.fold_left collect_typeables_var set (List.flatten var_blocks)
    in
    let rec collect_typeables_stmt set = function
      | Assignment (id, _) -> Typeables.add (Entity (Variable (name, id))) set
      | Output _ -> set
      | Error _ -> set
      | If (_, thn, Some els) ->
          collect_typeables_stmt (collect_typeables_stmt set thn) els
      | If (_, thn, None) -> collect_typeables_stmt set thn
      | While (_, body) -> List.fold_left collect_typeables_stmt set body
      | Store (id, _) -> Typeables.add (Entity (Variable (name, id))) set
      | DirectRecordWrite (id, _, _) ->
          Typeables.add (Entity (Variable (name, id))) set
      | Block body -> List.fold_left collect_typeables_stmt set body
    in

    let set = Typeables.add (Entity (Function name)) set in
    let set = collect_typeables_var_blocks set var_blocks in
    let set =
      List.fold_left
        (fun set arg -> Typeables.add (Entity (Variable (name, arg))) set)
        set args
    in
    List.fold_left collect_typeables_stmt set stmts
  in
  List.fold_left collect_typeables_func Typeables.empty program

let infer program =
  let typeables = collect_typeables program in
  Typeables.cardinal typeables

(* UnionFind structure specifically designed for typing. *)
module UnionFindX = struct
  type elem = varType
  type store = (varType, varType) Hashtbl.t

  let data : store = Hashtbl.create 10

  let add e =
    match Hashtbl.find_opt data e with
    | None -> Hashtbl.add data e e
    | Some _ -> ()

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

  let unionAdd a b =
    add a;
    add b;
    union a b
end

let typeInferenceUnion program =
  let unifyToplevelSignature { name; args; _ } =
    let args = List.map (fun arg -> Entity (Variable (name, arg))) args in
    List.iter UnionFindX.add args;
    UnionFindX.add (Entity (Retval name));

    let func_type = Arrow (args, Entity (Retval name))
    and func = Entity (Function name) in
    UnionFindX.add func_type;
    UnionFindX.add func;
    UnionFindX.union func func_type
  in
  List.iter unifyToplevelSignature program;

  let unifyFunctionBody { name; args; var_blocks; stmts; ret_expr } =
    let unifyExpression = failwith "unifyExpression unimplemented" in
    let rec unifyStatement = function
      | Assignment (id, expr) ->
          UnionFindX.unionAdd
            (Entity (Variable (name, id)))
            (unifyExpression expr)
      | Output (Int _) -> ()
      | Output Null -> failwith "Attempt to output Null"
      | Output (Id ident) ->
          UnionFindX.unionAdd (Entity (Variable (name, ident))) Int
      | Error (Int _) -> ()
      | Error Null -> failwith "Attempt to error Null"
      | Error (Id ident) ->
          UnionFindX.unionAdd (Entity (Variable (name, ident))) Int
      | If (cond, thn, els) -> (
          UnionFindX.unionAdd (unifyExpression cond) Int;
          unifyStatement thn;
          match els with None -> () | Some stmt -> unifyStatement stmt)
      | While (cond, body) ->
          UnionFindX.unionAdd (unifyExpression cond) Int;
          List.iter unifyStatement body
      | Store (id, expr) ->
          UnionFindX.unionAdd
            (Pointer (Entity (Variable (name, id))))
            (unifyExpression expr)
      | DirectRecordWrite (record, field, expr) -> failwith "unimplemented"
      | Block body -> List.iter unifyStatement body
    in

    List.iter unifyStatement stmts
  in
  List.iter unifyFunctionBody program;
  UnionFindX.data
