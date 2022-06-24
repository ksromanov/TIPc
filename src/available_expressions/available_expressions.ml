(* Reveal available expressions pass *)
module S_expressions = Set.Make (struct
  let compare = Stdlib.compare

  type t = Anf.complex_expression
end)

type available_expressions_lattice_t = S_expressions.t

(* Since the type is not a tree-like, it does not support blocks *)
type available_expressions_t =
  string * (Anf.statement * available_expressions_lattice_t) list

(* Print available_expressions_t in linear time *)
let show_available_expressions_t (name, annotated_statements) =
  let b = Buffer.create 10 in
  Buffer.add_string b ("Function " ^ name ^ "\n" ^ "");
  List.iter
    (fun (stmt, available_exprs) ->
      Buffer.add_string b (Anf.show_statement stmt ^ ":\n");
      Seq.iter
        (fun expr ->
          Buffer.add_string b ("\t" ^ Anf.show_complex_expression expr ^ ":\n"))
        (S_expressions.to_seq available_exprs))
    annotated_statements;
  Buffer.contents b

let analyze_function
    { Typed_anf.name : string; Typed_anf.stmts : Anf.statement list; _ } :
    available_expressions_t =
  let rec analyze_statement state = function
    | Anf.Assignment (id, Complex expr) as stmt ->
        (* remove all expressions, which contain the id *)
        let state' =
          S_expressions.add expr
          @@ S_expressions.filter
               (fun expr ->
                 match expr with
                 | Binop (Id a, _, Id b) when id = a || id = b -> true
                 | _ -> false)
               state
        in
        (state', (stmt, state))
    | Anf.Assignment (_, Atomic _) as stmt -> (state, (stmt, state))
    | Anf.Output _ as stmt -> (state, (stmt, state))
    | Anf.Error _ as stmt -> (state, (stmt, state))
    | Anf.If (_, thn, Some els) as ifstmt ->
        (* only then statement *)
        let thn_state, (_, _) = analyze_statement state thn in
        let els_state, (_, _) = analyze_statement state els in
        (S_expressions.union thn_state els_state, (ifstmt, state))
    | Anf.If (_, _, None) as stmt -> (state, (stmt, state))
    (* cond is atomic => we do not add it *)
    | Anf.While _ as stmt -> (state, (stmt, state))
    | Anf.Store _ as stmt ->
        (state, (stmt, state) (* we are not working with pointers here *))
    | Anf.DirectRecordWrite _ as stmt -> (state, (stmt, state))
    | Anf.Block body as stmt ->
        let state', _ = List.fold_left_map analyze_statement state body in
        (state', (stmt, state))
  in

  let (* final state *) _, result =
    List.fold_left_map analyze_statement S_expressions.empty stmts
  in

  (name, List.rev result)

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) : available_expressions_t list =
  List.map analyze_function program
