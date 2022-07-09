(* In this pass we are looking for uninitialized variables that are later
   used in the program. *)

(* The lattice per variable *)
type uninitialized_variables_lattice = Bottom | Top

(* Check if the variables are in state, if not return the name of the variable(s) *)
let analyze_atomic_expression state = function
  | Anf.Id (Anf.Ident var) -> Some var
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

  (* The result of the function is:
     1. the set of initialized vars
     2. the set of used but initialized vars *)
  let analyze_statement state = function
    | Anf.Assignment (id, Complex expr) as stmt -> failwith ""
    | Anf.Assignment (id, Atomic expr) as stmt -> failwith ""
    | Anf.Output atomic_expr -> failwith ""
    | Anf.Error atomic_expr -> failwith ""
    | Anf.If (cond, thn, Some els) -> failwith ""
    | Anf.If (cond, thn, None) -> failwith ""
    | Anf.While (cond, body) -> failwith ""
    | Anf.Store _ as stmt ->
        failwith "" (*state, stmt (* we are not working with pointers here *)*)
    | Anf.DirectRecordWrite _ as stmt -> failwith ""
    | Anf.Block body -> failwith ""
    (*
        let state, body = List.fold_left_map analyze_statement state body in
        (state, Anf.Block (List.rev body)) *)
  in
  (name, [])

type result = (string * string list) list

let analyze (program : Typed_anf.program) : result =
  List.map analyze_function program
