(* Type inference algorithm for TIPc language.
   We start from ANF representation. *)

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

(* Не факт, что будет использовать сейчас и в дальнейшем *)
module EntityType = Set.Make (struct
  type t = entityType

  let compare = compare
end)

(* Всевозможные объекты программы - функции, аргументы и переменные *)
type function_name = string [@@deriving show]

type entity =
  | Function of function_name
  | Argument of function_name * ident
  | Variable of function_name * ident
[@@deriving show]

(* Allocate or return type variable for a given entity *)
let typeVar_of_entity : entity -> typeVariable =
  let entity_type_var_map = Hashtbl.create 10 in
  let last_type_var_allocated = ref 0 in
  fun (e : entity) ->
    match Hashtbl.find_opt entity_type_var_map e with
    | Some t -> t
    | None ->
        last_type_var_allocated := !last_type_var_allocated + 1;
        Hashtbl.add entity_type_var_map e !last_type_var_allocated;
        !last_type_var_allocated

(* UnionFind structure specifically designed for typing. *)
module UnionFind = struct
  type elem = entityType

  let data : (entityType, entityType) Hashtbl.t = Hashtbl.create 10

  let make_set t =
    match Hashtbl.find_opt data t with
    | None -> Hashtbl.add data t t
    | Some _ -> ()

  let rec find t =
    match Hashtbl.find data t with r when r = t -> r | r -> find r

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

let collect_typeables program =
  let types : (entity, typeVariable) Hashtbl.t = Hashtbl.create 10 in
  let add_type_for_entity e =
    match Hashtbl.find_opt types e with
    | Some _ -> ()
    | None -> Hashtbl.add types e (typeVar_of_entity e)
  in

  let collect_typeables_func { name = f_name; args; var_blocks; stmts; _ } =
    let rec collect_typeables_stmt = function
      | Assignment (id, _) -> add_type_for_entity (Variable (f_name, id))
      | Output _ -> ()
      | Error _ -> ()
      | If (_, thn, Some els) ->
          collect_typeables_stmt thn;
          collect_typeables_stmt els
      | If (_, thn, None) -> collect_typeables_stmt thn
      | While (_, body) -> List.iter collect_typeables_stmt body
      | Store (id, _) -> add_type_for_entity (Variable (f_name, id))
      | DirectRecordWrite (id, _, _) ->
          add_type_for_entity (Variable (f_name, id))
      | Block body -> List.iter collect_typeables_stmt body
    in

    add_type_for_entity (Function f_name);
    List.iter (fun arg -> add_type_for_entity (Argument (f_name, arg))) args;

    List.iter (fun var -> add_type_for_entity (Variable (f_name, var)))
    @@ List.flatten var_blocks;
    List.iter collect_typeables_stmt stmts
  in
  List.iter collect_typeables_func program;
  types

let rec unify (a : entityType) (b : entityType) : unit =
  let open UnionFind in
  make_set a;
  make_set b;

  let a_r, b_r = (find a, find b) in
  match (a_r, b_r) with
  | _ when a_r = b_r -> ()
  | TypeVar _, TypeVar _ -> UnionFind.union a_r b_r
  | TypeVar _, _ -> UnionFind.union a_r b_r
  | _, TypeVar _ -> UnionFind.union b_r a_r
  | Int, Int -> failwith "unreachable"
  | Pointer p_a, Pointer p_b -> unify p_a p_b
  | Arrow (a_args, a_ret), Arrow (b_args, b_ret) ->
      List.iter2 unify a_args b_args;
      unify a_ret b_ret
  | Mu _, Mu _ -> failwith "mu functions are unreachable"
  | _ ->
      failwith @@ "unification failed between a = " ^ show_entityType a
      ^ " and " ^ show_entityType b

let typeInferenceUnion program = ()
(*  failwith "typeInferenceUnion is not yet implemented" *)

let infer program = Hashtbl.length @@ collect_typeables program
(* failwith "infer is unimplemented" *)

(*
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
*)
