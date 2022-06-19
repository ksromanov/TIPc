(* Reveal available expressions pass *)
module S_expressions = Set.Make (struct
  let compare = Stdlib.compare

  type t = Anf.complex_expression
end)

type available_expressions_lattice_t = S_expressions.t

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

let analyze_function _ = failwith "available_expressions unimplemented"

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) : available_expressions_t list =
  List.map analyze_function program
