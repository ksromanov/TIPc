(* Reveal available expressions pass *)

let analyze_function _ = failwith "unimplemented"

(* Main entry-point of the pass *)
let analyze (program : Typed_anf.program) : Typed_anf.program =
  List.map analyze_function program
