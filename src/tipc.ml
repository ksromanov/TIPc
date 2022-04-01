(* Driver for TIP parser *)
open Parsetree

let parse filename =
  let lexbuf = Lexing.from_channel (open_in filename) in

  try
    let program = Parser.program Lexer.token lexbuf in
    Printf.printf "Functions:\n";
    List.iter (fun f -> Printf.printf "\t%s\n%!" f.name) program
  with _ -> Printf.printf "Unknown error while parsing %s\n" filename

let _ =
  match Sys.argv with
  | [| _ |] -> Printf.eprintf "Please provide TIP file to parse.\n"
  | [| _; filename |] -> parse filename
  | _ -> Printf.eprintf "Please provide only one file to parse."
