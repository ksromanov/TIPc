(* Constant propagation pass for integer variables *)

(* Lattice for description of the value of an integer variable:
   Bottom — invalid state, i.e. no value, undefined variable
   Top — any integer value, Value — well defined integer constant *)
type constant_lattice = Bottom | Value of int | Top [@@deriving show]

let join a b =
  match (a, b) with
  | Bottom, b -> b
  | a, Bottom -> a
  | Top, _ | _, Top -> Top
  | Value a, Value b -> if a = b then Value a else Top

(* Joining Hashmaps using join above *)
let join_states a b =
  let merge_into result a =
    Seq.iter (fun (k, v) ->
        Hashtbl.add result k
          (join v @@ Option.value ~default:Bottom (Hashtbl.find_opt result k)))
    @@ Hashtbl.to_seq a
  in
  let result = Hashtbl.create 10 in
  merge_into result a;
  merge_into result b;
  result

(* Transfer function for binary operation *)
let eval_binop (op : Anf.binop) (a : constant_lattice) (b : constant_lattice) :
    constant_lattice =
  match (a, b) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top -> Top
  | Value a, Value b ->
      let int_of_bool = function true -> 1 | false -> 0 in
      let op =
        match op with
        | Plus -> ( + )
        | Minus -> ( - )
        | Times -> ( * )
        | Div -> ( / )
        | Greater -> fun a b -> int_of_bool (a > b)
        | Equal -> fun a b -> int_of_bool (a = b)
      in
      Value (op a b)

(* Fixed point algorithm more/less generic *)
let fix_point f state stmts =
  let rec iter state =
    let new_state, stmts = f (Hashtbl.copy state, stmts) in
    if new_state <> state then iter (join_states state new_state)
    else (new_state, stmts)
  in
  iter state

(* Each function is analyzed separately + we have some
   limitations, concerning pointers and records. I.e. int-only approximation. *)
let analyze_function
    ({
       Typed_anf.args : (Anf.argument * Typed_anf.entityType) list;
       Typed_anf.stmts : Anf.statement list;
       Typed_anf.ret_expr : Anf.atomic_expression;
       _;
     } as f) =
  let analyze_atomic_expression state_map = function
    | Anf.Int n -> Value n
    | Id ident -> Hashtbl.find state_map ident
    | Null -> Top
  in

  (* Here we analyze only binary operations! *)
  let analyze_complex_expression state_map = function
    | Anf.Binop (a, op, b) ->
        eval_binop op
          (analyze_atomic_expression state_map a)
          (analyze_atomic_expression state_map b)
    | Anf.Input -> Top
    | Anf.Apply _ -> Top
    | Anf.ComputedApply _ -> Top
    | Anf.Alloc _ -> Top
    | Anf.Reference _ -> Top
    | Anf.DeReference _ -> Top
    | Anf.Record _ -> Top
    | Anf.FieldRead _ -> Top
  in

  let rec analyze_statement state stmt =
    match stmt with
    | Anf.Assignment (id, Complex expr) as stmt -> (
        let expr_lattice = analyze_complex_expression state expr in
        Hashtbl.add state id expr_lattice;
        match expr_lattice with
        | Top | Bottom -> (state, stmt)
        | Value v -> (state, Anf.Assignment (id, Atomic (Anf.Int v))))
    | Anf.Assignment (id, Atomic expr) as stmt ->
        Hashtbl.add state id (analyze_atomic_expression state expr);
        (state, stmt)
    | Anf.Output atomic_expr -> (state, Anf.Output atomic_expr)
    | Anf.Error atomic_expr -> (state, Anf.Error atomic_expr)
    | Anf.If (cond, thn, Some els) -> (
        match analyze_atomic_expression state cond with
        | Top | Bottom ->
            let thn_state, thn = analyze_statement (Hashtbl.copy state) thn in
            let els_state, els = analyze_statement (Hashtbl.copy state) els in
            (join_states thn_state els_state, Anf.If (cond, thn, Some els))
        | Value 0 ->
            let els_state, els = analyze_statement state els in
            (els_state, Anf.If (Int 0, thn, Some els))
        | Value v ->
            let thn_state, thn = analyze_statement (Hashtbl.copy state) thn in
            (thn_state, Anf.If (Int v, thn, Some els)))
    | Anf.If (cond, thn, None) -> (
        let cond_value = analyze_atomic_expression state cond in
        let thn_state, thn = analyze_statement (Hashtbl.copy state) thn in
        match cond_value with
        | Top | Bottom -> (thn_state, Anf.If (cond, thn, None))
        | Value v -> (thn_state, Anf.If (Int v, thn, None)))
    | Anf.While (cond, body) -> (
        match analyze_atomic_expression state cond with
        | Value 0 -> (state, Anf.While (Int 0, body))
        | _ ->
            (* running overestimation *)
            let state, body =
              fix_point
                (fun (state, body) ->
                  List.fold_left_map analyze_statement state body)
                state body
            in
            (state, Anf.While (cond, body)))
    | Anf.Store _ as stmt ->
        (state, stmt (* we are not working with pointers here *))
    | Anf.DirectRecordWrite _ as stmt -> (state, stmt)
    | Anf.Block body ->
        let state, body = List.fold_left_map analyze_statement state body in
        (state, Anf.Block (List.rev body))
  in

  let state : (Anf.ident, constant_lattice) Hashtbl.t =
    Hashtbl.create (List.length args)
  in
  let state, stmtsi = List.fold_left_map analyze_statement state stmts in
  let ret_expri =
    match analyze_atomic_expression state ret_expr with
    | Top | Bottom -> ret_expr
    | Value v -> Int v
  in
  { f with Typed_anf.stmts = stmtsi; Typed_anf.ret_expr = ret_expri }

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) : Typed_anf.program =
  List.map analyze_function program
