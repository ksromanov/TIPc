(* Driver for TIP parser *)

let parse filename =
  let lexbuf = Lexing.from_channel (open_in filename) in

  try
    let program = Parser.program Lexer.token lexbuf in
    List.iter (Printf.printf "%d\n%!") program
  with
  | End_of_file -> Printf.printf "end of file"
  | _ -> Printf.printf "Unknown error while parsing %s\n" filename

let _ =
  match Sys.argv with
  | [| _ |] -> Printf.eprintf "Please provide TIP file to parse.\n"
  | [| _; filename |] -> parse filename
  | _ -> Printf.eprintf "Please provide only one file to parse."
