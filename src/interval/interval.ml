(* Interval analysis using widening and narrowing, without delta operator. *)
type value = Value of int | Neg_infty | Pos_infty [@@deriving show]

let less = function
  | _, Neg_infty -> false
  | Neg_infty, _ -> true
  | Pos_infty, _ -> false
  | _, Pos_infty -> true
  | Value a, Value b -> a < b

type interval = value * value [@@deriving show]
type interval_lattice_t = Bottom | Interval of interval [@@deriving show]

(* Top is encoded as an interval to avoid duplication *)
let top = Interval (Neg_infty, Pos_infty)
let is_top = ( = ) top

(* Simple join on the lattice values *)
let join a b =
  let join_intervals (a1, a2) (b1, b2) =
    let min a b = if less (a, b) then a else b in
    let max a b = if less (a, b) then b else a in
    (min a1 b1, max a2 b2)
  in
  match (a, b) with
  | Bottom, _ -> b
  | _, Bottom -> a
  | Interval a, Interval b -> Interval (join_intervals a b)

module VarIntervalHashtbl = struct
  type t = (Anf.ident, interval_lattice_t) Hashtbl.t

  let pp ppf values =
    Hashtbl.iter
      (fun key value ->
        Format.fprintf ppf "@[<1>%s: %s@]@." (Anf.show_ident key)
          (show_interval_lattice_t value))
      values

  let join
      (merge : interval_lattice_t -> interval_lattice_t -> interval_lattice_t)
      (a : t) (b : t) =
    let result = Hashtbl.copy a in
    let _ =
      Hashtbl.iter
        (fun k vb ->
          match Hashtbl.find_opt result k with
          | None -> ()
          | Some va -> Hashtbl.replace result k (merge va vb))
        b
    in
    result
end

(* The lattice for a single statement, state_t in other words. *)
type payload = VarIntervalHashtbl.t [@@deriving show]

(* join function for the lattice *)
let join_states (a : payload) (b : payload) = VarIntervalHashtbl.join join a b

type statement =
  | Assignment of Anf.ident * Anf.expression * payload
  | Output of Anf.atomic_expression * payload
  | Error of Anf.atomic_expression * payload
  | If of Anf.atomic_expression * statement * statement option * payload
  | While of Anf.atomic_expression * statement list * payload
  | Store of
      Anf.ident
      * Anf.expression (* Store value in a memory cell referenced by pointer *)
      * payload
  | DirectRecordWrite of Anf.ident * Anf.ident * Anf.expression * payload
  | Block of statement list
[@@deriving show]

(* Add default payload to the tree *)
let rec preset_statement (payload : unit -> payload) = function
  | Anf.Assignment (id, expr) -> Assignment (id, expr, payload ())
  | Anf.Output expr -> Output (expr, payload ())
  | Anf.Error expr -> Error (expr, payload ())
  | Anf.If (cond, thn, els) ->
      If
        ( cond,
          preset_statement payload thn,
          Option.map (preset_statement payload) els,
          payload () )
  | Anf.While (cond, body) ->
      While (cond, List.map (preset_statement payload) body, payload ())
  | Anf.Store (id, expr) -> Store (id, expr, payload ())
  | Anf.DirectRecordWrite (r, field, expr) ->
      DirectRecordWrite (r, field, expr, payload ())
  | Anf.Block body -> Block (List.map (preset_statement payload) body)

let analyze_expression state =
  let analyze_binop a b =
    match (a, b) with
    | Bottom, _ -> fun _ -> Bottom
    | _, Bottom -> fun _ -> Bottom
    | Interval (a1, a2), Interval (b1, b2) -> (
        function
        | Anf.Plus -> failwith "unimplemented"
        | Anf.Minus | Anf.Times | Anf.Div | Anf.Greater | Anf.Equal ->
            failwith "bin op unimplemented")
  in
  let analyze_atomic_expression state = function
    | Anf.Int i -> Interval (Value i, Value i)
    | Anf.Id id -> Option.value (Hashtbl.find_opt state id) ~default:Bottom
    | Anf.Null -> top
  in
  let analyze_complex_expression state = function
    | Anf.Binop (a, op, b) ->
        analyze_binop
          (analyze_atomic_expression state a)
          (analyze_atomic_expression state b)
          op
    | Anf.Input -> top
    | Anf.Apply _ -> top
    | Anf.ComputedApply _ -> top
    | Anf.Alloc _ -> Bottom
    | Anf.Reference _ -> Bottom
    | Anf.DeReference _ -> top
    | Anf.Record _ -> Bottom
    | Anf.FieldRead _ -> top
  in
  function
  | Anf.Atomic expr -> analyze_atomic_expression state expr
  | Anf.Complex expr -> analyze_complex_expression state expr

(* Create an approximation to currently available expressions
   without an account for pointers/records. *)
let analyze_function
    {
      Typed_anf.name : string;
      Typed_anf.args : (Anf.argument * Typed_anf.entityType) list;
      Typed_anf.var_blocks : (string * Typed_anf.entityType) list list;
      Typed_anf.stmts : Anf.statement list;
      Typed_anf.ret_expr : Anf.atomic_expression;
      _;
    } : unit =
  let rec analyze_statement previous_state = function
    (* remove all expressions, which contain the id *)
    | Assignment (id, expr, _) ->
        let state = Hashtbl.copy previous_state in
        let expr_interval = analyze_expression state expr in
        Hashtbl.replace state id expr_interval;
        (state, Assignment (id, expr, state))
    | Output (atomic_expr, _) ->
        (previous_state, Output (atomic_expr, Hashtbl.copy previous_state))
    | Error (atomic_expr, _) ->
        (previous_state, Error (atomic_expr, Hashtbl.copy previous_state))
    | If (cond, thn, els, _) -> (
        let thn_state, thn_result =
          analyze_statement (Hashtbl.copy previous_state) thn
        in
        match els with
        | None -> (thn_state, If (cond, thn, None, Hashtbl.copy previous_state))
        | Some els ->
            let els_state, els_result =
              analyze_statement (Hashtbl.copy previous_state) els
            in
            ( join_states thn_state els_state,
              If (cond, thn, Some els, Hashtbl.copy previous_state) ))
    | While (cond, body, _) -> failwith "Interval: unimplemented"
    | Store (_, _, _) as stmt -> (previous_state, stmt) (* unimplemented *)
    | DirectRecordWrite (_, _, _, _) as stmt ->
        (previous_state, stmt) (* unimplemented *)
    | Block body ->
        let state, body =
          List.fold_left_map analyze_statement previous_state body
        in
        (state, Block body)
  in
  let stmts = List.map (preset_statement (fun () -> Hashtbl.create 10)) stmts in
  let _ = List.fold_left_map analyze_statement (Hashtbl.create 10) stmts in
  ()

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) = List.map analyze_function program