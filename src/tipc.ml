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
  Printf.printf "ANF form: %s\n" (Anf.show_program anf_program);

  let types = Typing.infer anf_program in
  Printf.printf "%d independent variables" types;
  ()

let _ =
  match Sys.argv with
  | [| _ |] -> Printf.eprintf "Please provide TIP file to parse.\n"
  | [| _; filename |] -> parse filename
  | _ -> Printf.eprintf "Please provide only one file to parse."
