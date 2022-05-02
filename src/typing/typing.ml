(* Type inference algorithm for TIPc language.
   We start from ANF representation. *)

open Anf
open Typed_anf

(* Всевозможные объекты программы - функции, аргументы и переменные *)
type function_name = string [@@deriving show]

type entity =
  | Function of function_name
  | Argument of function_name * ident
  | Return of function_name
  | Variable of function_name * ident
[@@deriving show]

let void _ = ()

(* Allocate or return type variable for a given entity *)
let ( (create_typevar : unit -> typeVariable),
      (typevar_of_entity : entity -> typeVariable),
      (typed_entities : function_name -> entity list) ) =
  let entity_type_var_map = Hashtbl.create 10 in
  let last_type_var_allocated = ref 0 in
  ( (fun () ->
      last_type_var_allocated := !last_type_var_allocated + 1;
      !last_type_var_allocated),
    (fun (e : entity) ->
      match Hashtbl.find_opt entity_type_var_map e with
      | Some t -> t
      | None ->
          last_type_var_allocated := !last_type_var_allocated + 1;
          Hashtbl.add entity_type_var_map e !last_type_var_allocated;
          !last_type_var_allocated),
    fun f_name ->
      Hashtbl.fold
        (fun e _ acc ->
          match e with
          | Function name as e when name = f_name -> e :: acc
          | Argument (name, _) as e when name = f_name -> e :: acc
          | Return name as e when name = f_name -> e :: acc
          | Variable (name, _) as e when name = f_name -> e :: acc
          | _ -> acc)
        entity_type_var_map [] )

(* UnionFind structure specifically designed for typing. *)
module UnionFind = struct
  type elem = entityType

  let data : (entityType, entityType) Hashtbl.t = Hashtbl.create 10

  let make_set t =
    match Hashtbl.find_opt data t with
    | None -> Hashtbl.add data t t
    | Some _ -> ()

  let contains t = Option.is_some @@ Hashtbl.find_opt data t

  let rec find t =
    match Hashtbl.find data t with r when r = t -> r | r -> find r

  let union a b =
    let b_root = find b in
    let rec update_root t r =
      match Hashtbl.find data t with
      | r' when r' = t -> Hashtbl.add data t r
      | r' ->
          Hashtbl.add data t r;
          update_root r' r
    in
    update_root a b_root
end

let collect_typeables program =
  let types : (entity, typeVariable) Hashtbl.t = Hashtbl.create 10 in
  let add_type_for_entity e =
    match Hashtbl.find_opt types e with
    | Some _ -> ()
    | None -> Hashtbl.add types e (typevar_of_entity e)
  in

  let collect_typeables_func
      ({ name = f_name; args; var_blocks; stmts; _ } : Anf.func) =
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

    List.iter (fun var -> add_type_for_entity (Variable (f_name, Ident var)))
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
  | Int, Int -> failwith "unreachable: Int, Int comparison"
  | Pointer p_a, Pointer p_b -> unify p_a p_b
  | Arrow (a_args, a_ret), Arrow (b_args, b_ret) ->
      if List.length a_args <> List.length b_args then
        failwith @@ "Unable to unify " ^ show_entityType a_r ^ " and "
        ^ show_entityType b_r ^ " because of different arities";
      List.iter2 unify a_args b_args;
      unify a_ret b_ret
  | Mu _, Mu _ -> failwith "unreachable: mu functions"
  | _ ->
      let show r = function
        | TypeVar _ as e -> show_entityType e ^ " -> " ^ show_entityType r
        | e -> show_entityType e
      in
      failwith @@ "unification failed between a = " ^ show a_r a ^ " and b = "
      ^ show b_r b

let typeInferenceUnion (program : Anf.program) =
  let entityType_of_entity (e : entity) : entityType =
    let typeVar = TypeVar (typevar_of_entity e) in
    UnionFind.make_set typeVar;
    typeVar
  in

  let make_typevar e = void @@ entityType_of_entity e in

  let infer_types_of_function ({ name; args; var_blocks; stmts; _ } : Anf.func)
      =
    let module VarSet = Set.Make (String) in
    (* Names from variable block to allow "shadowing" of arguments *)
    let declared_var_names =
      List.fold_left (fun v set -> VarSet.add set v) VarSet.empty
      @@ List.flatten var_blocks
    in
    let is_declared_var ident = VarSet.mem ident declared_var_names in
    let entityType_of_ident (i : ident) =
      match i with
      | Ident id when is_declared_var id ->
          entityType_of_entity (Variable (name, i))
      | Temporary _ -> entityType_of_entity (Variable (name, i))
      | _ -> entityType_of_entity (Argument (name, i))
    in

    let infer_type_of_atomic_expression = function
      | Anf.Int _ -> Int
      | Id ident -> entityType_of_ident ident
      | Anf.Null -> Pointer Int (* FIXME: add "new_typevar" *)
    in
    let infer_type_of_complex_expression = function
      | Binop (a, _, b) ->
          unify
            (infer_type_of_atomic_expression a)
            (infer_type_of_atomic_expression b);
          Int
      | Input -> Int
      | Apply (Temporary _, _) ->
          failwith "internal error: Apply of temporary variable"
      | Apply (Ident f_ident, _)
        when not (List.exists (fun { Anf.name; _ } -> name = f_ident) program)
        ->
          failwith "internal error: Application of an undeclared function"
      | Apply (Ident f_ident, args) -> (
          let f_type = TypeVar (typevar_of_entity (Function f_ident)) in
          match UnionFind.find f_type with
          | Arrow (a_args, ret) ->
              List.iter2 unify a_args
              @@ List.map infer_type_of_atomic_expression args;
              ret
          | _ -> failwith "internal error - function is tied with non-Arrow")
      | ComputedApply (f_ident, args) ->
          let f_type = entityType_of_ident f_ident in
          let arg_types = List.map infer_type_of_atomic_expression args in
          let ret_type = TypeVar (create_typevar ()) in
          UnionFind.make_set ret_type;
          List.iter UnionFind.make_set arg_types;
          (* FIXME: is it mandatory? *)
          unify f_type (Arrow (arg_types, ret_type));
          ret_type
      | Alloc expr -> Pointer (infer_type_of_atomic_expression expr)
      | Reference ident -> Pointer (entityType_of_ident ident)
      | DeReference expr -> failwith "internal error: unreachable"
      | Record r -> failwith "record is not yet implemented"
      | FieldRead (Id r, field) -> failwith "Field read is not implemented"
      | FieldRead (_, _) -> failwith "Attempt to read non-record"
    in
    let infer_types_of_expression = function
      | Atomic expr -> infer_type_of_atomic_expression expr
      | Complex expr -> infer_type_of_complex_expression expr
    in
    let rec infer_types_of_statement = function
      | Assignment (ident, expr) -> (
          match expr with
          (* Dereference should be considered separately *)
          | Complex (DeReference expr) ->
              unify
                (Pointer (entityType_of_ident ident))
                (infer_type_of_atomic_expression expr)
          | _ ->
              unify (entityType_of_ident ident) (infer_types_of_expression expr)
          )
      | Output (Int _) | Error (Int _) -> ()
      | Output Null -> failwith "Attempt to output NULL"
      | Error Null -> failwith "Attempt to make an error of NULL"
      | Output (Id ident) | Error (Id ident) ->
          unify (entityType_of_ident ident) Int
      | If (cnd, thn, els) ->
          unify (infer_type_of_atomic_expression cnd) Int;
          infer_types_of_statement thn;
          void @@ Option.map infer_types_of_statement els
      | While (cnd, body) ->
          unify (infer_type_of_atomic_expression cnd) Int;
          List.iter infer_types_of_statement body
      | Store (ident, expr) ->
          let exprType = infer_types_of_expression expr in
          unify (entityType_of_ident ident) (Pointer exprType)
      | DirectRecordWrite (record, field, expr) ->
          failwith "DirectRecordWrite unimplemented"
      | Block stmts -> List.iter infer_types_of_statement stmts
    in

    List.iter (fun var -> make_typevar (Variable (name, Ident var)))
    @@ List.flatten var_blocks;

    List.iter infer_types_of_statement stmts
  in

  let add_function_signature ({ name; args; _ } : Anf.func) =
    let args =
      List.map (fun arg -> entityType_of_entity (Argument (name, arg))) args
    in
    List.iter UnionFind.make_set args;
    let ret = entityType_of_entity (Return name) in
    UnionFind.make_set ret;
    let func_signature = Arrow (args, ret) in
    UnionFind.make_set func_signature;
    let func = entityType_of_entity (Function name) in
    UnionFind.make_set func;
    UnionFind.union func func_signature
  in

  (* adding function signatures before processing bodies *)
  List.iter add_function_signature program;
  List.iter infer_types_of_function program

let infer (program : Anf.program) : Typed_anf.program =
  typeInferenceUnion program;
  List.map
    (fun { Anf.name; Anf.args; Anf.var_blocks; Anf.stmts; Anf.ret_expr } ->
      {
        Typed_anf.name;
        Typed_anf.args =
          List.map
            (fun a ->
              ( a,
                UnionFind.find
                  (TypeVar (typevar_of_entity (Argument (name, a)))) ))
            args;
        Typed_anf.var_blocks =
          List.map
            (List.map (fun v ->
                 ( v,
                   UnionFind.find
                     (TypeVar (typevar_of_entity (Variable (name, Ident v)))) )))
            var_blocks;
        Typed_anf.stmts;
        Typed_anf.ret_expr;
        Typed_anf.return_type =
          UnionFind.find (TypeVar (typevar_of_entity (Function name)));
        Typed_anf.temporary_vars =
          List.map (fun t ->
              ( t,
                UnionFind.find
                  (TypeVar (typevar_of_entity (Variable (name, Temporary t))))
              ))
          @@ List.filter_map (function
               | Variable (_, Temporary t) -> Some t
               | _ -> None)
          @@ typed_entities name;
      })
    program
