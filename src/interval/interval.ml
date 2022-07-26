(* Interval analysis using widening and narrowing, without delta operator. *)
type value = Value of int | Neg_infty | Pos_infty [@@deriving show]
type interval = value * value [@@deriving show]
type interval_lattice_t = Bottom | Interval of interval [@@deriving show]

(* Top is encoded as an interval to avoid duplication *)
let top = Interval (Neg_infty, Pos_infty)
let is_top = (=) top

module VarIntervalHashtbl = struct
  type t = (string, interval_lattice_t) Hashtbl.t

  let pp ppf values =
    Hashtbl.iter (fun key value ->
      Format.fprintf ppf "@[<1>%s: %s@]@." key (show_interval_lattice_t value)) values
end

(* The lattice for a single statement, state_t in other words. *)
type payload = VarIntervalHashtbl [@@deriving show]

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
let rec preset_statement (payload:payload)  =
  function
  | Anf.Assignment (id, expr) -> Assignment (id, expr, payload)
  | Anf.Output expr -> Output (expr, payload)
  | Anf.Error expr -> Error (expr, payload)
  | Anf.If (cond, thn, els) ->
      If (cond, preset_statement payload thn, Option.map (preset_statement payload ) els, payload)
  | Anf.While (cond, body) ->
      While (cond, List.map (preset_statement payload) body, payload)
  | Anf.Store (id, expr) -> Store (id, expr, payload)
  | Anf.DirectRecordWrite (r, field, expr) ->
      DirectRecordWrite (r, field, expr, payload)
  | Anf.Block body -> Block (List.map (preset_statement payload) body)

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
    | Assignment (id, expr, _) -> failwith "Interval: unimplemented"
    | Output (atomic_expr, _) -> failwith "Interval: unimplemented"
    | Error (atomic_expr, _) -> failwith "Interval: unimplemented"
    | If (cond, thn, els, _) -> failwith "Interval: unimplemented"
    | While (cond, body, _) -> failwith "Interval: unimplemented"
    | Store (id, expr, _) -> failwith "Interval: unimplemented"
    | DirectRecordWrite (r, field, expr, _) ->
        failwith "Interval: unimplemented"
    | Block body ->
        let state, body =
          List.fold_left_map analyze_statement previous_state body
        in
        (state, Block body)
  in
  let _ = List.fold_left_map analyze_statement () (failwith "" (*stmts*)) in
  ()

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) = List.map analyze_function program
