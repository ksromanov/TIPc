(* Simple sign analysis, based on 5.1 of the book *)

(* Lattice for description of the sign of an integer variable:
   Bottom — invalid state, i.e. no value, analog of NaN
   Top — any number, either positive or negative *)
type sign_lattice = Bottom | Positive | Negative | Zero | Top
[@@deriving show]

(* Name of the function, signs of all ids for all statements, return expression sign *)
type sign_analysis_t =
  string * (Anf.statement * (Anf.ident * sign_lattice) list) list * sign_lattice
[@@deriving show]

let join a b =
  match (a, b) with
  | Bottom, b -> b
  | a, Bottom -> a
  | Top, _ -> Top
  | _, Top -> Top
  | _, _ when a = b -> a
  | _, _ -> Top

let eval_binop (op : Anf.binop) (a : sign_lattice) (b : sign_lattice) =
  (* Processing only "real" numbers 9x9 lattice *)
  let plus a b =
    match (a, b) with
    | Positive, Positive -> Positive
    | Negative, Negative -> Negative
    | Zero, b -> b
    | a, Zero -> a
    | Positive, Negative -> Top
    | Negative, Positive -> Top
    | _, _ -> failwith "Unreachable in plus"
  in
  let minus a b =
    match (a, b) with
    | Positive, Positive -> Top
    | Negative, Negative -> Top
    | Zero, Positive -> Negative
    | Zero, Negative -> Positive
    | _, Zero -> a
    | Positive, Negative -> Positive
    | Negative, Positive -> Negative
    | _, _ -> failwith "Unreachable in minus"
  in
  let times a b =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | Positive, Positive -> Positive
    | Negative, Negative -> Positive
    | Negative, Positive -> Negative
    | Positive, Negative -> Positive
    | _, _ -> failwith "Unreachable in times"
  in
  let div a b =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> Bottom
    | Positive, Positive -> Positive
    | Positive, Negative -> Negative
    | Negative, Positive -> Negative
    | Negative, Negative -> Negative
    | _, _ -> failwith "Unreachable in div"
  in
  let greater a b =
    match (a, b) with
    | Positive, Positive -> Top
    | Positive, _ -> Positive
    | Negative, Negative -> Top
    | Negative, _ -> Zero
    | Zero, Negative -> Positive
    | Zero, _ -> Zero
    | _, _ -> failwith "Unreachable in greater"
  in
  let equal a b =
    match (a, b) with
    | Zero, Zero -> Positive
    | _, _ when a = b -> Top
    | _ -> Zero
  in
  match (a, b) with
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> (
      match (op, a, b) with
      | Anf.Times, Zero, _ -> Zero
      | Anf.Times, _, Zero -> Zero
      | _, Top, _ -> Top
      | _, _, Top -> Top
      | Anf.Plus, _, _ -> plus a b
      | Anf.Minus, _, _ -> minus a b
      | Anf.Times, _, _ -> times a b
      | Anf.Div, _, _ -> div a b
      | Anf.Greater, _, _ -> greater a b
      | Anf.Equal, _, _ -> equal a b)

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) : sign_analysis_t list =
  let open Typed_anf in
  let open Anf in
  let analyse_atomic_expression state_map = function
    | Int 0 -> Zero
    | Int n when n > 0 -> Positive
    | Int _ -> Negative (* n < 0, however typechecker can't infer it *)
    | Id ident -> Hashtbl.find state_map ident
    | Null -> Top
  in

  (* Here we analyze only binary operations! *)
  let analyse_complex_expression state_map = function
    | Anf.Binop (a, op, b) ->
        eval_binop op
          (analyse_atomic_expression state_map a)
          (analyse_atomic_expression state_map b)
    | Anf.Input -> Top
    | Anf.Apply _ -> Top
    | Anf.ComputedApply _ -> Top
    | Anf.Alloc _ -> Top
    | Anf.Reference _ -> Top
    | Anf.DeReference _ -> Top
    | Anf.Record _ -> Top
    | Anf.FieldRead _ -> Top
  in

  (* Analyze statement, returning updated result *)
  let rec analyze_statement type_map ((state_map, states) as result) = function
    | Anf.Assignment (ident, Complex expr) as stmt ->
        Hashtbl.add state_map ident (analyse_complex_expression state_map expr);
        (state_map, (stmt, Hashtbl.copy state_map) :: states)
    | Anf.Assignment (ident, Atomic expr) as stmt ->
        Hashtbl.add state_map ident (analyse_atomic_expression state_map expr);
        (state_map, (stmt, Hashtbl.copy state_map) :: states)
    | Anf.Output _ -> result (* Nothing to do *)
    | Anf.Error _ -> result
    | Anf.If (cond, thn, els) as stmt -> (
        match analyse_atomic_expression state_map cond with
        | Positive | Negative -> analyze_statement type_map result thn
        | Zero ->
            Option.value
              (Option.map (analyze_statement type_map result) els)
              ~default:result
        | Top | Bottom -> (
            match els with
            | None -> analyze_statement type_map result thn
            | Some els ->
                let thn_state_map = Hashtbl.copy state_map in
                let els_state_map = Hashtbl.copy state_map in
                let _ =
                  analyze_statement type_map (thn_state_map, snd result) thn
                in
                let _ =
                  analyze_statement type_map (els_state_map, snd result) els
                in
                let joint_state_map =
                  Hashtbl.of_seq
                  @@ Seq.map2
                       (fun (a_key, a_state) (b_key, b_state) ->
                         if a_key <> b_key then
                           failwith "Keys are different in Join then and else"
                         else (a_key, join a_state b_state))
                       (Hashtbl.to_seq thn_state_map)
                       (Hashtbl.to_seq els_state_map)
                in
                ( joint_state_map,
                  (stmt, Hashtbl.copy joint_state_map) :: snd result )))
    | Anf.While (cond, _) when analyse_atomic_expression state_map cond = Zero
      ->
        result (* skipping, since condition does not let us enter the loop!!! *)
    | Anf.While (_, _) -> failwith "unimplemented"
    | Anf.Store _ -> result (* pointers are not yet considered! *)
    | Anf.DirectRecordWrite _ -> result (* unimplemented *)
    | Anf.Block body -> List.fold_left (analyze_statement type_map) result body
  in
  let analyze_function
      {
        name : string;
        args : (argument * entityType) list;
        var_blocks : (string * entityType) list list;
        stmts : statement list;
        ret_expr : atomic_expression;
        (* Additional type information *)
        temporary_vars : (int * entityType) list;
        _;
      } : sign_analysis_t =
    let initial_state_map, type_map =
      let reserve_length =
        List.length args
        + (List.fold_left ( + ) 0 @@ List.map List.length var_blocks)
        + List.length temporary_vars
      in
      let state_map, type_map =
        (Hashtbl.create reserve_length, Hashtbl.create reserve_length)
      in

      List.iter
        (fun (e, typ) ->
          Hashtbl.add state_map e Top;
          Hashtbl.add type_map e typ)
        args;
      List.iter
        (List.iter (fun (v, typ) ->
             Hashtbl.add state_map (Ident v) Top;
             Hashtbl.add type_map (Ident v) typ))
        var_blocks;
      List.iter
        (fun (t, typ) ->
          Hashtbl.add state_map (Temporary t) Top;
          Hashtbl.add type_map (Temporary t) typ)
        temporary_vars;
      (state_map, type_map)
    in
    let final_state, statement_results =
      List.fold_left (analyze_statement type_map) (initial_state_map, []) stmts
    in
    ( name,
      List.rev_map
        (fun (stmt, signs) -> (stmt, List.of_seq @@ Hashtbl.to_seq signs))
        statement_results,
      analyse_atomic_expression final_state ret_expr )
  in
  List.map analyze_function program
