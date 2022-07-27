(* Driver for TIP parser *)

let parse filename =
  let lexbuf = Lexing.from_channel (open_in filename) in

  (*  try *)
  let program = Parser.program Lexer.token lexbuf in
  (*  Printf.printf "%s\n" (show_program program); *)
  (*   with _ -> Printf.printf "Unknown error while parsing %s\n" filename; *)
  let result = Parsetree_interpreter.run program [] in
  Printf.printf "Result: %s\n" (Parsetree_interpreter.show_value result);
  Printf.printf
    "--------------------------------------------------------------------------------\n";
  let anf_program = Anf_of_parsetree.anf_of_parsetree program in
  let typed_anf_program = Typing.infer anf_program in
  Printf.printf "Typed ANF form: %s\n"
    (Typed_anf.show_program typed_anf_program);
  (*
  let sign_analysis_result = Sign_analysis.analyze typed_anf_program in
  Printf.printf "Sign analysis:\n";
  List.iter
    (fun signs ->
      Printf.printf "%s\n" (Sign_analysis.show_sign_analysis_t signs))
    sign_analysis_result;
  let typed_anf_program_after_constprop =
    Constant_propagation.analyze typed_anf_program
  in
  Printf.printf "Typed ANF form after constant propagation: %s\n"
    (Typed_anf.show_program typed_anf_program_after_constprop);
*)
  (*
  let available_expressions = Available_expressions.analyze typed_anf_program in
  Printf.printf "Available expressions:\n";
  List.iter
    (fun e ->
      Printf.printf "%s\n"
        (Available_expressions.show_available_expressions_t e))
    available_expressions;
*)
  let very_busy_expressions = Very_busy_expressions.analyze typed_anf_program in
  Printf.printf "Very busy expressions:\n";
  List.iter
    (fun (name, stmts) ->
      Printf.printf "Function %s:\n" name;
      List.iter
        (fun s ->
          Printf.printf "%s\n" (Very_busy_expressions.show_busy_statement s))
        stmts)
    very_busy_expressions;
  let uninitialized_variables =
    Uninitialized_variables.analyze typed_anf_program
  in
  Printf.printf "Uninitialized variables:\n%s\n"
    (Uninitialized_variables.show_result uninitialized_variables);
  let interval_analysis_results = Interval.analyze typed_anf_program in
  Printf.printf "Interval analysis:\n%s\n"
    (Interval.show_interval_analysis_result_t interval_analysis_results)

let _ =
  match Sys.argv with
  | [| _ |] -> Printf.eprintf "Please provide TIP file to parse.\n"
  | [| _; filename |] -> parse filename
  | _ -> Printf.eprintf "Please provide only one file to parse."
