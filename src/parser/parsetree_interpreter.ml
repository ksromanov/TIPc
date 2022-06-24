open Parsetree

let int_of_bool b = if b then 1 else 0

type eval_result =
  | OkEval of int * int list (* ret code + output *)
  | ErrorEval of int (* program triggered error *)
  | FailEvaluation (* evaluation failed *)

type pointer = int [@@deriving show]

type value =
  | RecordValue of record
  | Integer of int
  | Pointer of pointer
  | Function of ident
  | Null
[@@deriving show]

type heap = (pointer, value) Hashtbl.t

let alloc =
  let max_pointer_value = ref 0 in
  fun (h : heap) (v : value) ->
    while Hashtbl.mem h !max_pointer_value do
      max_pointer_value := !max_pointer_value + 1
    done;
    Hashtbl.add h !max_pointer_value v;
    !max_pointer_value

type environment = {
  h : heap;
  vars : (ident * pointer) list;
  program : program; (* parse tree of the program *)
}

let create_environment program = { h = Hashtbl.create 20; vars = []; program }

(* Evaluate binary operation :: value -> bin_op -> value -> value *)
let eval_binop a op b =
  let a, b =
    match (a, b) with
    | Integer a, Integer b -> (a, b)
    | _ -> failwith "Non integer in eval_binop"
  in
  Integer
    (match op with
    | Plus -> a + b
    | Minus -> a - b
    | Times -> a * b
    | Div -> a / b
    | Greater -> int_of_bool (a > b)
    | Equal -> int_of_bool (a = b))

(* Evaluate expression :: expression -> value *)
let rec eval (env : environment) (e : expression) =
  match e with
  | Int n -> Integer n
  | Id id -> (
      match List.assoc_opt id env.vars with
      | Some pointer -> Hashtbl.find env.h pointer
      | None -> (
          match List.find_opt (fun f -> f.name = id) env.program with
          | None -> failwith ("Unable to find function '" ^ id ^ "' in apply.")
          | Some f -> Function f.name))
  | Binop (a, Equal, b) -> Integer (int_of_bool (eval env a = eval env b))
  | Binop (a, op, b) -> eval_binop (eval env a) op (eval env b)
  | Input -> Integer 6 (* failwith "not implemented (input)"*)
  | Parenthesized e -> eval env e
  | DirectApply (f, args) -> apply env f (List.map (eval env) args)
  | ComputedApply (fx, args) -> (
      match eval env fx with
      | Function f -> apply env f (List.map (eval env) args)
      | _ -> failwith "Attempt to call a non-function")
  | Alloc e -> Pointer (alloc env.h (eval env e))
  | Reference id -> (
      match List.assoc_opt id env.vars with
      | Some pointer -> Pointer pointer
      | _ ->
          failwith @@ "Attempt to get reference of undeclared variable '" ^ id
          ^ "'")
  | DeReference e -> (
      match eval env e with
      | Pointer p -> Hashtbl.find env.h p
      | _ -> failwith "expression evaluates not to a pointer")
  | Null -> Integer 0
  | Record r -> RecordValue r
  | FieldRead (e, id) -> (
      match eval env e with
      | RecordValue r -> eval env (List.assoc id r)
      | _ -> failwith ("trying to access not a record's field '" ^ id ^ "'"))

(* Apply function and return result as expression *)
and apply (env : environment) (id : ident) (args : value list) =
  match List.find_opt (fun f -> f.name = id) env.program with
  | None -> failwith ("Unable to find function '" ^ id ^ "' in apply.")
  | Some f ->
      let arg_vars = List.combine f.args (List.map (alloc env.h) args) in
      let var_blocks =
        List.map (fun id -> (id, alloc env.h (Integer 0)))
        @@ List.flatten f.var_blocks
      in
      let env = { env with vars = arg_vars @ var_blocks } in
      let env = List.fold_left exec env f.stmts in
      eval env f.ret_expr

(* Execute statement, returns environment *)
and exec (env : environment) (s : statement) =
  match s with
  | Assignment (id, e) -> (
      match List.assoc_opt id env.vars with
      | Some pointer ->
          Hashtbl.add env.h pointer (eval env e);
          env
      | None -> failwith @@ "Undeclared variable '" ^ id ^ "'")
  | Output e ->
      Printf.printf "Output: %s\n" (show_expression e);
      env
  | Error e -> failwith @@ "Program triggered error " ^ show_value (eval env e)
  | If (cond, thn, els) -> (
      match (eval env cond, els) with
      | Integer 1, _ -> exec env thn
      | _, Some els -> exec env els
      | _, None -> env)
  | While (cond, body) ->
      let env = ref env in
      while eval !env cond = Integer 1 do
        env := List.fold_left exec !env body
      done;
      !env
  (* Store value in a memory cell referenced by pointer *)
  | Store (ptr_e, e) -> (
      match eval env ptr_e with
      | Pointer p ->
          Hashtbl.remove env.h p;
          Hashtbl.add env.h p (eval env e);
          env
      | _ -> failwith "Attempt to store to non-ptr")
  (*
  | DirectRecordWrite of ident * ident * expression
  | IndirectRecordWrite of expression * ident * expression
 *)
  | Block body -> List.fold_left exec env body
  | _ -> failwith "Parsetree: unimplemented"

(* val eval : program -> int list -> result *)
let run program _ =
  match List.find_opt (fun f -> f.name = "main") program with
  | Some _ -> apply (create_environment program) "main" []
  | _ -> Null
