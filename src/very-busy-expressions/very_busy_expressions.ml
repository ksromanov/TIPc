(* Reveal busy expressions pass. In contrast to available expressions
   pass we are storing data for each AST node in a Hashmap *)
(* Temporary storage of available expressions with printing *)
module S_expressions = struct
  include Set.Make (struct
    let compare = Stdlib.compare

    type t = Anf.complex_expression
  end)

  let pp fmt s =
    Format.fprintf fmt "%s" @@ [%show: Anf.complex_expression list] (elements s)
end

(* Remove all expressions, containing specific variable *)
let filter_out_variable id =
  S_expressions.filter (fun expr ->
      match expr with
      | Binop (Id a, _, Id b) when id = a || id = b -> true
      | _ -> false)

(* Resulting type of the expression *)
type payload = S_expressions.t [@@deriving show]

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

type func = {
  name : string;
  args : (Anf.argument * Typed_anf.entityType) list;
  var_blocks : (string * Typed_anf.entityType) list list;
  stmts : statement list;
  ret_expr : Anf.atomic_expression;
}
[@@deriving show]

(* Add default payload to the tree *)
let rec preset_statement : Anf.statement -> statement =
  let payload = S_expressions.empty in
  function
  | Anf.Assignment (id, expr) -> Assignment (id, expr, payload)
  | Anf.Output expr -> Output (expr, payload)
  | Anf.Error expr -> Error (expr, payload)
  | Anf.If (cond, thn, els) ->
      If (cond, preset_statement thn, Option.map preset_statement els, payload)
  | Anf.While (cond, body) ->
      While (cond, List.map preset_statement body, payload)
  | Anf.Store (id, expr) -> Store (id, expr, payload)
  | Anf.DirectRecordWrite (r, field, expr) ->
      DirectRecordWrite (r, field, expr, payload)
  | Anf.Block body -> Block (List.map preset_statement body)

(* Вопрос - как сделать 0-е приближение, а потом делать нормальный
   worklist алгоритм *)
let analyze_function
    {
      Typed_anf.name : string;
      Typed_anf.args : (Anf.argument * Typed_anf.entityType) list;
      Typed_anf.var_blocks : (string * Typed_anf.entityType) list list;
      Typed_anf.stmts : Anf.statement list;
      Typed_anf.ret_expr : Anf.atomic_expression;
      _;
    } : func =
  let remove_id id =
    S_expressions.filter (fun expr ->
        match expr with
        | Binop (Id a, _, Id b) when id = a || id = b -> true
        | _ -> false)
  in

  (* We deal only with complex expressions. *)
  let rec analyze_statement previous_state = function
    (* remove all expressions, which contain the id *)
    | Assignment (id, Complex expr, _) ->
        let state' = S_expressions.add expr @@ remove_id id previous_state in
        (state', Assignment (id, Complex expr, previous_state))
    | Assignment (id, Atomic expr, _) ->
        let state' = remove_id id previous_state in
        (state', Assignment (id, Atomic expr, previous_state))
    | Output (atomic_expr, _) ->
        (previous_state, Output (atomic_expr, previous_state))
    | Error (atomic_expr, _) ->
        (previous_state, Error (atomic_expr, previous_state))
    | If (cond, thn, els, _) ->
        let thn_state, thn = analyze_statement previous_state thn in
        let els_state, els =
          match Option.map (analyze_statement previous_state) els with
          | None -> (previous_state, None)
          | Some (state, els) -> (state, Some els)
        in
        ( S_expressions.inter thn_state els_state,
          If (cond, thn, els, previous_state) )
    | While (cond, body, _) ->
        (* FIXME: add fixpoint *)
        let state, body =
          List.fold_left_map analyze_statement previous_state body
        in
        (state, While (cond, body, previous_state))
    | Store (id, expr, _) -> (previous_state, Store (id, expr, previous_state))
    | DirectRecordWrite (r, field, expr, _) ->
        (previous_state, DirectRecordWrite (r, field, expr, previous_state))
    | Block body ->
        let state, body =
          List.fold_left_map analyze_statement previous_state body
        in
        (state, Block body)
  in
  {
    name;
    args;
    var_blocks;
    stmts =
      snd
      @@ List.fold_left_map analyze_statement S_expressions.empty
      @@ List.map preset_statement stmts;
    ret_expr;
  }

type busy = Busy | Regular [@@deriving show]

type busy_statement =
  | Assignment of Anf.ident * Anf.expression * busy
  | Output of Anf.atomic_expression
  | Error of Anf.atomic_expression
  | If of Anf.atomic_expression * busy_statement * busy_statement option
  | While of Anf.atomic_expression * busy_statement list
  | Store of
      Anf.ident
      * Anf.expression (* Store value in a memory cell referenced by pointer *)
      * busy
  | DirectRecordWrite of Anf.ident * Anf.ident * Anf.expression * busy
  | Block of busy_statement list
[@@deriving show]

(* Deriving busy flag based on the accumulated statements. *)
let rec derive_busy_flags : statement -> busy_statement =
  let is_busy state = function
    | Anf.Complex expr when S_expressions.mem expr state -> Busy
    | _ -> Regular
  in
  function
  | Assignment (id, expr, state) -> Assignment (id, expr, is_busy state expr)
  | Output (expr, _) -> Output expr
  | Error (expr, _) -> Error expr
  | If (cond, thn, els, _) ->
      If (cond, derive_busy_flags thn, Option.map derive_busy_flags els)
  | While (cond, body, _) -> While (cond, List.map derive_busy_flags body)
  | Store (id, expr, state) -> Store (id, expr, is_busy state expr)
  | DirectRecordWrite (r, f, expr, state) ->
      DirectRecordWrite (r, f, expr, is_busy state expr)
  | Block body -> Block (List.map derive_busy_flags body)

type result = (string * busy_statement list) list [@@deriving show]

let analyze (program : Typed_anf.program) : result =
  let analysis_results = List.map analyze_function program in
  List.map
    (fun f -> (f.name, List.map derive_busy_flags f.stmts))
    analysis_results
