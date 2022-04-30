(* Translation of the program into ANF style *)
open Anf

let anf_binop = function
  | Parsetree.Plus -> Plus
  | Parsetree.Minus -> Minus
  | Parsetree.Times -> Times
  | Parsetree.Div -> Div
  | Parsetree.Greater -> Greater
  | Parsetree.Equal -> Equal

let make_temp_ident =
  let current = ref 0 in
  fun () ->
    current := !current + 1;
    Temporary !current

let rec anf_atomic_expression expr =
  let assignments, ret_expr = anf_expression expr in
  match ret_expr with
  | Atomic ret_expr -> (assignments, ret_expr)
  | Complex _ ->
      let ident = make_temp_ident () in
      (assignments @ [ (ident, ret_expr) ], Id ident)

and anf_expression expr =
  match expr with
  | Parsetree.Int n -> ([], Atomic (Int n))
  | Parsetree.Id id -> ([], Atomic (Id (Ident id)))
  | Parsetree.Binop (a, op, b) ->
      let assignments_a, atomic_a = anf_atomic_expression a in
      let assignments_b, atomic_b = anf_atomic_expression b in
      ( assignments_a @ assignments_b,
        Complex (Binop (atomic_a, anf_binop op, atomic_b)) )
  | Parsetree.Input -> ([], Complex Input)
  | Parsetree.Parenthesized expr -> anf_expression expr
  | DirectApply (f, args) ->
      let assignments, args =
        List.split @@ List.map anf_atomic_expression args
      in
      (List.concat assignments, Complex (Apply (Ident f, args)))
  | ComputedApply (f, args) -> (
      let f_assignments, f = anf_atomic_expression f in
      let assignments, args =
        List.split @@ List.map anf_atomic_expression args
      in
      match f with
      | Id f ->
          ( f_assignments @ List.concat assignments,
            Complex (ComputedApply (f, args)) )
      | _ -> failwith "ANF: Attempt to call a non-function")
  | Alloc expr ->
      let assignments, id = anf_atomic_expression expr in
      (assignments, Complex (Alloc id))
  | Reference ident -> ([], Complex (Reference (Ident ident)))
  | DeReference expr ->
      let assignments, atomic = anf_atomic_expression expr in
      (assignments, Complex (DeReference atomic))
  | Null -> ([], Atomic Null)
  | Record r ->
      let field_ids, exprs = List.split r in
      let assignments, atomic_exprs =
        List.split @@ List.map anf_atomic_expression exprs
      in
      let field_ids = List.map (fun id -> Ident id) field_ids in
      ( List.concat assignments,
        Complex (Record (List.combine field_ids atomic_exprs)) )
  | FieldRead (expr, field_id) ->
      let assignments, id = anf_atomic_expression expr in
      (assignments, Complex (FieldRead (id, Ident field_id)))

let statement_of_assignment (id, expr) = Assignment (id, expr)

let statement_of_atomic_expr expr statement_of_expr =
  let assignments, ret = anf_expression expr in
  match ret with
  | Atomic ret ->
      List.map statement_of_assignment assignments @ [ statement_of_expr ret ]
  | _ -> failwith "ANF: internal error, non-atomic expression"

let rec anf_of_parsetree_stmt stmt =
  let open Parsetree in
  match stmt with
  | Assignment (id, expr) ->
      let assignments, ret = anf_expression expr in
      List.map statement_of_assignment assignments
      @ [ Assignment (Ident id, ret) ]
  | Output expr -> statement_of_atomic_expr expr (fun e -> Output e)
  | Error expr -> statement_of_atomic_expr expr (fun e -> Error e)
  | If (cond, thn, els) ->
      let assignments, atomic_cond = anf_atomic_expression cond in
      List.map statement_of_assignment assignments
      @ [
          If
            ( atomic_cond,
              Block (anf_of_parsetree_stmt thn),
              match els with
              | Some els -> Some (Block (anf_of_parsetree_stmt els))
              | None -> None );
        ]
  | While (cond, body) ->
      let assignments, atomic_cond = anf_atomic_expression cond in
      List.map statement_of_assignment assignments
      @ [
          While (atomic_cond, List.flatten (List.map anf_of_parsetree_stmt body));
        ]
  | Store (ptr, expr) -> (
      let assignments, ret = anf_expression expr in
      let ptr_assignments, ptr_id = anf_atomic_expression ptr in
      match ptr_id with
      | Id id ->
          List.map statement_of_assignment assignments
          @ List.map statement_of_assignment ptr_assignments
          @ [ Store (id, ret) ]
      | _ -> failwith "ANF: Attempt to store to non-ptr")
  | DirectRecordWrite (r, f, expr) ->
      let assignments, ret = anf_expression expr in
      List.map statement_of_assignment assignments
      @ [ DirectRecordWrite (Ident r, Ident f, ret) ]
  | IndirectRecordWrite (rexpr, f, expr) -> (
      let assignments, ret = anf_expression expr in
      let r_assignments, r_id = anf_atomic_expression rexpr in
      match r_id with
      | Id id ->
          List.map statement_of_assignment assignments
          @ List.map statement_of_assignment r_assignments
          @ [ DirectRecordWrite (id, Ident f, ret) ]
      | _ -> failwith "ANF: Attempt to store to non-record")
  | Block stmts -> List.flatten @@ List.map anf_of_parsetree_stmt stmts

let anf_of_parsetree_func func =
  let ret_assignments, ret_expr =
    anf_atomic_expression func.Parsetree.ret_expr
  in
  {
    name = func.Parsetree.name;
    args = List.map (fun s -> Ident s) func.Parsetree.args;
    var_blocks = func.Parsetree.var_blocks;
    stmts =
      (List.flatten @@ List.map anf_of_parsetree_stmt func.Parsetree.stmts)
      @ List.map statement_of_assignment ret_assignments;
    ret_expr;
  }

let anf_of_parsetree = List.map anf_of_parsetree_func
