(* In this pass we are looking for uninitialized variables that are later
   used in the program. *)

(* The lattice per variable *)
type uninitialized_variables_lattice = Bottom | Top
type state_t = (string, uninitialized_variables_lattice) Hashtbl.t

(* We need to assume that variables are initialized only if they are
   intialized in both branches. *)
let join a b = match (a, b) with Top, Top -> Top | _ -> Bottom

let join_maps a b =
  Hashtbl.of_seq
  @@ Seq.map2
       (fun (ka, va) (kb, vb) ->
         if ka <> kb then failwith "Different keys in uninitialized variables"
         else (ka, join va vb))
       (Hashtbl.to_seq a) (Hashtbl.to_seq b)

(* Check if the variables are in state, if not return the name of the variable(s) *)
let analyze_atomic_expression state = function
  | Anf.Id (Anf.Ident var) -> (
      match Hashtbl.find state var with Bottom -> Some var | Top -> None)
  | _ -> None

let analyze_complex_expression state =
  let filter_unoption n = List.concat @@ List.map Option.to_list n in
  function
  | Anf.Binop (a, _, b) ->
      filter_unoption
        [ analyze_atomic_expression state a; analyze_atomic_expression state b ]
  | Anf.Input -> []
  | Anf.Apply (_, vars) ->
      filter_unoption @@ List.map (analyze_atomic_expression state) vars
  | Anf.ComputedApply (_, vars) ->
      filter_unoption @@ List.map (analyze_atomic_expression state) vars
  | Anf.Alloc expr -> Option.to_list @@ analyze_atomic_expression state expr
  | Anf.Reference ident ->
      Option.to_list @@ analyze_atomic_expression state (Anf.Id ident)
  | Anf.DeReference expr ->
      Option.to_list @@ analyze_atomic_expression state expr
  | Anf.Record _ -> []
  | Anf.FieldRead (expr, _) ->
      Option.to_list @@ analyze_atomic_expression state expr

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
    } : string * string list =
  let vars : string list = List.map fst @@ List.concat var_blocks in

  let state : state_t =
    Hashtbl.of_seq @@ List.to_seq @@ List.map (fun v -> (v, Bottom)) vars
  in

  (* The result of the function is:
     1. the set of initialized vars
     2. the set of used but initialized vars *)
  let rec analyze_statement (state : state_t) = function
    | Anf.Assignment (Anf.Ident id, Complex expr) ->
        let uninitialized_vars = analyze_complex_expression state expr in
        Hashtbl.replace state id Top;
        (state, uninitialized_vars)
    | Anf.Assignment (_, Complex expr) ->
        (state, analyze_complex_expression state expr)
    | Anf.Assignment (Anf.Ident id, Atomic expr) ->
        let uninitialized_var = analyze_atomic_expression state expr in
        Hashtbl.replace state id Top;
        (state, Option.to_list uninitialized_var)
    | Anf.Assignment (_, Atomic expr) ->
        (state, Option.to_list @@ analyze_atomic_expression state expr)
    | Anf.Output atomic_expr ->
        (state, Option.to_list @@ analyze_atomic_expression state atomic_expr)
    | Anf.Error atomic_expr ->
        (state, Option.to_list @@ analyze_atomic_expression state atomic_expr)
    | Anf.If (cond, thn, Some els) ->
        let cond = Option.to_list @@ analyze_atomic_expression state cond in
        let then_state, then_uninitialized_vars =
          analyze_statement (Hashtbl.copy state) thn
        in
        let else_state, else_uninitialized_vars =
          analyze_statement (Hashtbl.copy state) els
        in
        ( join_maps then_state else_state,
          cond @ then_uninitialized_vars @ else_uninitialized_vars )
    | Anf.If (cond, thn, None) ->
        let cond = Option.to_list @@ analyze_atomic_expression state cond in
        let _, uninitialized_vars =
          analyze_statement (Hashtbl.copy state) thn
        in
        (state, cond @ uninitialized_vars)
    | Anf.While (cond, body) ->
        (state, []) (* we are not working with pointers here *)
    | Anf.Store _ as stmt ->
        (state, []) (* we are not working with pointers here *)
    | Anf.DirectRecordWrite _ ->
        (state, []) (* we are not working with pointers here *)
    | Anf.Block body ->
        let state, uninitialized_vars =
          List.fold_left_map analyze_statement state body
        in
        (state, List.sort_uniq compare @@ List.concat uninitialized_vars)
  in
  let final_state, uninitialized_vars =
    List.fold_left_map analyze_statement state stmts
  in
  let ret_unused_variable =
    Option.to_list @@ analyze_atomic_expression final_state ret_expr
  in
  ( name,
    List.sort_uniq compare
    @@ List.concat (ret_unused_variable :: uninitialized_vars) )

type result = (string * string list) list [@@deriving show]

let analyze (program : Typed_anf.program) : result =
  List.map analyze_function program
