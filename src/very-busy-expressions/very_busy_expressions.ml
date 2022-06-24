let analyze_function (_ : Typed_anf.func) : unit = ()

let analyze (program : Typed_anf.program) : unit list =
  List.map analyze_function program
